# System design

## User experience

When a user enters Jukebox, they can either create new a room or join an existing room (using the room's ID). To invite other users into their room, they can send the room's link to another user.

A room has an audience of zero or more users. When a user enters a room, they are assigned a unique client ID that identifies them. They also have a username, which defaults to their client ID. A user can change their own user ID whenever they want.

Users can add videos to the queue. Whoever adds a video is its "submitter". Videos from the queue play one at a time, with playback synchronized between all users in the room. The submitter of the video controls the playback of the video (i.e. playing, pausing, and seeking).

When a video is playing, users that are not the submitter can "upvote" or "downvote" the video. The video's "net score" is its upvotes minus its downvotes. A video whose net score is less than `ceil(-1 * 0.5 * [the number of users in the room])` is immediately skipped as soon as the score threshold is reached.

## Backend architecture

The backend needs to actively manage two things: rooms and clients.

A room has some state, an inbox, and a list of client outboxes. When a room is created, the backend forks a thread that manages the room. This room manager listens for messages on the room's inbox, processes the messages, and sends reply messages to the client's outboxes as needed.

A client joins by opening a WebSocket connection to the server. The connection's path must be the path of the room that the client is joining. Clients must join existing rooms (they cannot create rooms during the joining process, or connect without specifying a room).

When the client joins, an outbox is created for the client, and added to the list of outboxes of the client's room. The client is also given the room's inbox. Two threads are then forked:
1. The first thread listens for WebSocket messages from the client. When it receives a message, it sends the message into the room's inbox for the room manager to process.
2. The second thread listens for messages in the client's outbox. When it receives a message, it sends the message to the client.

Since these are all lightweight GHC threads, performance is acceptable. All session information is stored in-memory.

## Messaging protocol and state machine

### State

Rooms track the following state in the backend:

- A room ID.
- A list of connected clients.
- An "active" video, if one exists. This includes:
  - The video's URL.
  - The video's playback status. The playback status can be either `Paused` or `Playing`. Both statuses track the current seek point when they were entered. The `Playing` status also tracks when playback started (for synchronizing clients joining mid-playback).
  - The "finished status" of each client for this video.
  - The video's votes (upvote/downvote and voter).
- A list of queued videos (both the URL and submitter).

Clients track the following state in the backend:

- A client ID.
- A client username.

### Events

This documentation is not (yet) exhaustive. It serves primarily to document the most complex parts of this protocol.

#### Received by the server

When a client joins a room:

- The room receives `ClientJoined` (from the handler).
- The room sends `UpdateClientList` to all connected clients. Previously connected clients are informed of the new client's arrival, and the new client is given a full list of the room's connected clients.
- The room's queue is either empty or non-empty. A non-empty queue implies there is a video currently playing (there can never be a non-empty queue with no current video playing). If the queue is non-empty, the room sends `UpdateQueue` to the new client. This sets the new client's queue.
- The room either has an active video, or does not. If the room has an active video, it sends `SetPlayer` to the new client. This synchronizes the new client's player.

When a client adds a video to the queue:

- The client sends `AddToQueue` to the room.
- If the queue is currently empty, the client's video immediately becomes the active video. See "when the active video changes".

When a client finishes playback of a video:

- The client sends `PlaybackFinished` to the room.
- When all clients have sent `PlaybackFinished`:
  - If there is no next video in the queue, the room clears the active video.
  - If there is a next video in the queue, the room replaces the active video with the next video in the queue. See "when the active video changes".
- (Waiting for `PlaybackFinished` ensures that clients that are slightly behind will not be cut off early.)

When a client plays, pauses, or seeks a video:

- Clients that know they are not the submitter of the video should not send these messages. If they send these messages anyway, their authorization is still validated in the backend.
- The client sends `RequestPlay`, `RequestPause`, or `RequestSeek` to the room.
- If the client is the submitter of the active video, the room will process the request:
  - The room will send `SetPlayer` to all clients. See "when a client receives `SetPlayer`".

When the active video changes:

- The room replaces the previous video with the new active video.
- The room sends `UpdateQueue` to all clients, updating their queues.
- The room sends `SetPlayer` to all clients, updating their active videos and playback statuses.

#### Received by the client

When a client receives `SetPlayer`:

- The client updates its local player's playback status.
- The client sends `PlaybackStarted`
- Clients _must not_ change local playback status without first seeing `SetPlayer`.
