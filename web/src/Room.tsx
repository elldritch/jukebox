import { useState, useEffect, useRef } from "react";

import useWebSocket, { ReadyState } from "react-use-websocket";
import formatDuration from "format-duration";
import { XMarkIcon } from "@heroicons/react/20/solid";

import Player, { PlayerController } from "./Player";

// TODO: Can we generate these type definitions from the Aeson instances?
type ClientID = string;

type Client = {
  handle: string;
  clientID: ClientID;
};

type UpdateClientList = {
  tag: "UpdateClientList";
  clients: Client[];
  you: Client;
};

type SetPlayer = {
  tag: "SetPlayer";
  submitter: ClientID;
  videoID: string;
  playbackStatus: PlaybackStatus;
};

type UnsetPlayer = {
  tag: "UnsetPlayer";
};

type PlaybackStatus =
  | {
      tag: "Playing";
      started: string;
      fromSeekSeconds: number;
    }
  | {
      tag: "Paused";
      atSeekSeconds: number;
    };

type UpdateQueue = {
  tag: "UpdateQueue";
  videos: QueuedVideo[];
};

type QueuedVideo = {
  videoID: string;
  submitter: ClientID;
};

type UpdateVotes = {
  tag: "UpdateVotes";
  skips: number;
};

type ServerMessage = UpdateClientList | UpdateQueue | UpdateVotes | SetPlayer | UnsetPlayer;

const SECONDS_DRIFT_ALLOWED = 2;

export default function Room() {
  // Connect to WebSocket.
  const { sendJsonMessage, lastJsonMessage, readyState } = useWebSocket<ServerMessage>(document.location.pathname);

  // Hooks related to player health.
  // const playerRef = useRef<ReturnType<typeof YouTubePlayer> | null>(null);
  const playerRef = useRef<PlayerController | null>(null);
  const [playerReady, setPlayerReady] = useState(false);
  const [errorMessage, setErrorMessage] = useState<React.JSX.Element | null>(null);
  const [fatalError, setFatalError] = useState<{} | undefined>(undefined);

  // Hooks related to playback.
  const [playerVolume, setPlayerVolume] = useState(100);
  const [playerMuted, setPlayerMuted] = useState(false);
  // The desired `playing` status. We maintain this to quickly override user
  // inputs in the player using `setTimeout(..., 1)` when needed.
  const [playingDesired, setPlayingDesired] = useState(false);
  // The actual `playing` status reflecting the player's state. Note that this
  // can go out of sync with the player if the player callbacks are not
  // respected.
  //
  // We have this in addition to `playingDesired` because we want to track the
  // actual playing status so that we know when it needs to be changed. If we
  // only ever tracked the desired playing status, we would not have a prop to
  // change when the user changed the actual status but not the desired status
  // (e.g. if they click on the player). We need a changeable prop because the
  // `Player` only updates when one of its props _changes_.
  const [playingActual, setPlayingActual] = useState(false);

  // Hooks related to the active video.
  const [hasActiveVideo, setHasActiveVideo] = useState(false);
  const [playerURL, setPlayerURL] = useState("");
  const [playerHostID, setPlayerHostID] = useState("");
  const [activeVideoDuration, setActiveVideoDuration] = useState<number | undefined>(undefined);
  const [activeVideoPlayedSeconds, setActiveVideoPlayedSeconds] = useState(0);

  // Hooks used to track remote seek synchronization.
  const [lastPlayerMessage, setLastPlayerMessage] = useState<SetPlayer | UnsetPlayer | undefined>(undefined);

  // Hooks related to the local seeking control.
  const [isSeeking, setIsSeeking] = useState(false);
  const [isSeekingValue, setIsSeekingValue] = useState(0);

  // Hooks related to the client list.
  const [clients, setClients] = useState<Client[]>([]);
  const [myClientID, setMyClientID] = useState("");
  const [newHandleInput, setNewHandleInput] = useState("");

  // Hooks related to the queue.
  const [addToQueueInput, setAddToQueueInput] = useState("");
  const [queuedVideos, setQueuedVideos] = useState<QueuedVideo[]>([]);

  // Hooks related to skip voting.
  const [votedToSkip, setVotedToSkip] = useState(false);
  const [skipVotes, setSkipVotes] = useState(0);

  function syncSeek(msg: SetPlayer) {
    console.log("syncSeek", msg);
    let seekToSeconds = 0;
    switch (msg.playbackStatus.tag) {
      case "Playing": {
        // Calculate correct seek, compensating for start time.
        const started = new Date(msg.playbackStatus.started);
        const now = new Date();
        const elapsedSeconds = Math.max(0, (now.getTime() - started.getTime()) / 1000);
        seekToSeconds = elapsedSeconds + msg.playbackStatus.fromSeekSeconds;
        break;
      }
      case "Paused": {
        seekToSeconds = msg.playbackStatus.atSeekSeconds;
        break;
      }
      default: {
        const _exhaustiveCheck: never = msg.playbackStatus;
        console.error("Unknown playback status", _exhaustiveCheck);
      }
    }
    console.log("Seek calculation", { seekToSeconds, activeVideoPlayedSeconds });
    // Allow clients to drift a tiny little bit. Otherwise they will
    // jump a bit to try and snap to the "correct" seek.
    if (Math.abs(seekToSeconds - activeVideoPlayedSeconds) > SECONDS_DRIFT_ALLOWED) {
      if (playerRef.current) {
        playerRef.current.seekTo(seekToSeconds, true);
        setActiveVideoPlayedSeconds(seekToSeconds);
      }
    }
  }

  // Manage WebSocket messages. Using `useEffect` deduplicates the
  // `lastJsonMessage`s.
  useEffect(() => {
    const msg = lastJsonMessage;
    if (msg) {
      console.log("WebSocket message", msg);

      switch (msg.tag) {
        case "UpdateClientList": {
          setClients(msg.clients);
          setMyClientID(msg.you.clientID);
          break;
        }
        case "UpdateQueue": {
          setQueuedVideos(msg.videos);
          break;
        }
        case "UpdateVotes": {
          setSkipVotes(msg.skips);
          break;
        }
        case "SetPlayer": {
          setLastPlayerMessage(msg);
          setHasActiveVideo(true);
          setPlayerURL(msg.videoID);
          setPlayerHostID(msg.submitter);
          if (msg.videoID !== playerURL) {
            setVotedToSkip(false);
            setSkipVotes(0);
          }
          console.log("SetPlayer: syncSeek", msg);
          syncSeek(msg);
          switch (msg.playbackStatus.tag) {
            case "Playing": {
              setPlayingDesired(true);
              setPlayingActual(true);
              break;
            }
            case "Paused": {
              setPlayingDesired(false);
              setPlayingActual(false);
              break;
            }
            default: {
              const _exhaustiveCheck: never = msg.playbackStatus;
              console.error("Unknown playback status", _exhaustiveCheck);
            }
          }
          break;
        }
        case "UnsetPlayer": {
          setLastPlayerMessage(msg);
          setHasActiveVideo(false);
          setPlayerURL("");
          setPlayerHostID("");
          setPlayingDesired(false);
          setPlayingActual(false);
          setActiveVideoPlayedSeconds(0);
          setActiveVideoDuration(undefined);
          setVotedToSkip(false);
          setSkipVotes(0);
          break;
        }
        default: {
          const _exhaustiveCheck: never = msg;
          console.error("Unknown message", _exhaustiveCheck);
        }
      }
    }
  }, [lastJsonMessage]);

  // Render disconnected state. Note that all returns must occur after all hooks
  // are called, so that all hooks are called in the same order every render.
  const connectionStatus = {
    [ReadyState.CONNECTING]: "connecting",
    [ReadyState.OPEN]: "open",
    [ReadyState.CLOSING]: "closing",
    [ReadyState.CLOSED]: "closed",
    [ReadyState.UNINSTANTIATED]: "uninstantiated",
  }[readyState];
  if (readyState === ReadyState.CONNECTING) {
    return (
      <div className="mx-auto max-w-lg mt-8">
        One moment, connecting to server. Current status: <code>{connectionStatus}</code>
      </div>
    );
  } else if (readyState !== ReadyState.OPEN) {
    return (
      <div className="mx-auto max-w-lg mt-8">
        Lost connection to server. Current status: <code>{connectionStatus}</code>
      </div>
    );
  }

  // Render error state.
  if (fatalError) {
    console.error(fatalError);
    return (
      <div className="mx-auto max-w-lg mt-8 text-red-700 border-l-4 border-red-400 bg-red-50 p-4">
        <p>
          Whoa! Something broke.{" "}
          <a href="" className="font-medium underline">
            Try reloading?
          </a>
        </p>
        <p className="mt-2">
          Here's the error: <code>{JSON.stringify(fatalError, null, 2)}</code>
        </p>
      </div>
    );
  }

  // Render happy path.
  const handle = (clientID: ClientID) => clients.find((c) => c.clientID === clientID)?.handle;
  const formatSeconds = (seconds: number) => formatDuration(seconds * 1000);

  return (
    <div className="mx-auto max-w-2xl mt-8">
      <Player
        videoId={playerURL}
        playing={playingActual}
        volume={playerVolume}
        muted={playerMuted}
        onReady={(e) => {
          console.log("onReady", { e, playingDesired, playingActual });
          playerRef.current = e.target;
          setPlayerReady(true);
          if (playingDesired) {
            setPlayingActual(true);
          }
          const duration = playerRef.current.getDuration();
          console.log("onReady: setActiveVideoDuration", duration);
          setActiveVideoDuration(duration);
          if (duration) {
            // This fixes a UI glitch that can occur when the user joins a room
            // with an active video where a client has spent longer than the
            // duration of the video without sending `PlaybackFinished`. In this
            // case, the "video seconds played" will momentarily be calculated
            // to be longer than the video's duration.
            //
            // This can happen if a client in the room has an extremely bad
            // network connection.
            if (activeVideoPlayedSeconds > duration) {
              setActiveVideoPlayedSeconds(duration);
            }
          }
        }}
        onPlay={() => {
          console.log("onPlay", { playingActual, playingDesired });
          // For some reason, this does not work during `onReady`. I think it's
          // because I can call seek only after the video (as opposed to the
          // player) is loaded? But YouTube, in their infinite wisdom, have
          // decided not to give us a lifecycle event for that. Hooray!
          //
          // This handles the case where a control message is sent before the
          // player is initialized.
          if (lastPlayerMessage && lastPlayerMessage.tag === "SetPlayer" && myClientID !== playerHostID) {
            syncSeek(lastPlayerMessage);
          }
          setPlayingActual(true);
          if (!playingDesired) {
            setTimeout(() => setPlayingActual(false), 1);
          }
          sendJsonMessage({ tag: "PlaybackStarted" });
        }}
        onPause={() => {
          console.log("onPause", { playingDesired });
          setPlayingActual(false);
          if (playingDesired) {
            setTimeout(() => setPlayingActual(true), 1);
          }
        }}
        onProgress={({ playedSeconds }) => {
          setActiveVideoPlayedSeconds(playedSeconds);
          // We continue trying to set duration during play because sometimes it
          // isn't available in `onReady`, and returns `undefined` instead.
          setActiveVideoDuration(playerRef.current!.getDuration());
        }}
        onEnded={() => {
          console.log("onEnded");
          if (activeVideoDuration) {
            // This is needed because the progress doesn't fire every frame.
            setActiveVideoPlayedSeconds(activeVideoDuration);
          }
          sendJsonMessage({ tag: "PlaybackFinished" });
        }}
        onAutoplayBlocked={() => {
          // If this site does not have autoplay permissions for this user,
          // begin autoplay on mute.
          console.log("onAutoplayBlocked");
          setErrorMessage(
            <>
              <a href="https://developer.chrome.com/blog/autoplay/" target="_blank" className="font-medium underline">
                Chrome blocks autoplaying with sound
              </a>
              , so you may need to unmute or click the video to start playing.
            </>
          );
          setPlayingActual(false);
          setPlayerMuted(true);
          setTimeout(() => setPlayingActual(true), 1);
        }}
        onError={(err) => {
          console.error(err);
          setErrorMessage(
            <>
              <p>The player encountered an error, so we skipped that video. Here's the error:</p>
              <code className="inline-block mt-2">{JSON.stringify(err, null, 2)}</code>
            </>
          );
          sendJsonMessage({ tag: "PlaybackFinished" });
        }}

        // TODO: Add callbacks for onBuffer and onBufferEnd and use them for
        // synchronization after buffering? So far, everyone's latency has been
        // good enough that this has not been needed.
      />
      <div className="mt-4">
        <p>
          {hasActiveVideo ? (
            <>
              Current host:{" "}
              {handle(playerHostID) ? (
                <>
                  {handle(playerHostID)}
                  {playerHostID === myClientID && " (you)"}
                </>
              ) : (
                <code>ERROR: UNKNOWN HOST: {JSON.stringify(playerHostID)}</code>
              )}
            </>
          ) : (
            "No video playing"
          )}
        </p>
      </div>
      {hasActiveVideo && (
        <>
          <div className="mt-2 grid grid-cols-[auto_auto_auto_1fr_auto] gap-2 items-center">
            {playerHostID === myClientID ? (
              <>
                <div>Room controls:</div>
                <div className="min-w-20">
                  <button
                    type="button"
                    className="rounded-md bg-gray-100 px-3 py-1.5 text-sm text-gray-900 shadow-sm hover:bg-gray-200"
                    onClick={() => {
                      const newPlayingDesired = !playingDesired;
                      console.log("TogglePlay.onClick: setPlayingDesired", { newPlayingDesired });
                      setPlayingDesired(newPlayingDesired);
                      if (newPlayingDesired !== playingActual) {
                        setPlayingActual(newPlayingDesired);
                      }
                      if (newPlayingDesired) {
                        sendJsonMessage({
                          tag: "RequestPlay",
                          fromSeekSeconds: Math.round(activeVideoPlayedSeconds),
                        });
                      } else {
                        sendJsonMessage({
                          tag: "RequestPause",
                          atSeekSeconds: Math.round(activeVideoPlayedSeconds),
                        });
                      }
                    }}
                  >
                    {playingDesired ? "Pause" : "Play"}
                  </button>
                </div>
                <span className="ml-2 text-sm">Seek</span>
                <input
                  className="ml-2 inline-block align-middle"
                  type="range"
                  min="0"
                  value={isSeeking ? isSeekingValue : activeVideoPlayedSeconds}
                  max={activeVideoDuration || 1}
                  step="any"
                  disabled={!(hasActiveVideo && playerReady && activeVideoDuration)}
                  onMouseDown={() => setIsSeeking(true)}
                  onChange={(e) => setIsSeekingValue(Number(e.target.value))}
                  onMouseUp={() => {
                    playerRef.current?.seekTo(isSeekingValue, true);
                    setActiveVideoPlayedSeconds(isSeekingValue);
                    setIsSeeking(false);
                    if (playingDesired) {
                      sendJsonMessage({
                        tag: "RequestPlay",
                        fromSeekSeconds: Math.round(isSeekingValue),
                      });
                    } else {
                      sendJsonMessage({
                        tag: "RequestPause",
                        atSeekSeconds: Math.round(isSeekingValue),
                      });
                    }
                  }}
                />
                <span className="ml-2 text-sm">
                  {activeVideoDuration &&
                    `${formatSeconds(activeVideoPlayedSeconds)} / ${formatSeconds(activeVideoDuration)}`}
                </span>
              </>
            ) : (
              <>
                <input
                  className="ml-2 inline-block align-middle col-span-4"
                  type="range"
                  min="0"
                  value={activeVideoPlayedSeconds}
                  max={activeVideoDuration || 1}
                  step="any"
                  disabled
                />
                <span className="ml-2 text-sm">
                  {activeVideoDuration &&
                    `${formatSeconds(Math.ceil(activeVideoPlayedSeconds))} / ${formatSeconds(activeVideoDuration)}`}
                </span>
              </>
            )}
            <div>Your controls:</div>
            <div className="min-w-20">
              <button
                type="button"
                className="rounded-md bg-gray-100 px-3 py-1.5 text-sm text-gray-900 shadow-sm hover:bg-gray-200"
                onClick={() => setPlayerMuted(!playerMuted)}
              >
                {playerMuted ? "Unmute" : "Mute"}
              </button>
            </div>
            <span className="ml-2 text-sm">Volume</span>
            <input
              className="ml-2 inline-block align-middle"
              type="range"
              min="0"
              max="100"
              step="1"
              value={playerVolume}
              onChange={(e) => setPlayerVolume(Number(e.target.value))}
              disabled={!playerReady}
            />
            <span className="ml-2 text-sm">{playerVolume + "%"}</span>
          </div>
          {errorMessage && (
            <div className="text-sm text-red-700 border-l-4 border-red-400 bg-red-50 p-4 mt-2 flex">
              <div>{errorMessage}</div>
              <div className="ml-auto pl-3">
                <div className="-mx-1.5 -my-1.5">
                  <button
                    type="button"
                    className="inline-flex rounded-md bg-red-50 p-1.5 text-red-500 hover:bg-red-100 focus:outline-none focus:ring-2 focus:ring-red-600 focus:ring-offset-2 focus:ring-offset-red-50"
                    onClick={() => setErrorMessage(null)}
                  >
                    <span className="sr-only">Dismiss</span>
                    <XMarkIcon aria-hidden="true" className="size-5" />
                  </button>
                </div>
              </div>
            </div>
          )}
        </>
      )}
      <div className="mt-4 grid grid-cols-2 gap-x-4">
        <div>
          <form
            className="flex rounded-md shadow-sm"
            onSubmit={(e) => {
              e.preventDefault();
              let slug = addToQueueInput.trim();
              try {
                const url = new URL(addToQueueInput);
                let param = url.searchParams.get('v');
                if (param) {
                  slug = param;
                }
              } catch (error) {}
              sendJsonMessage({
                tag: "AddToQueue",
                videoID: slug,
              });
              setAddToQueueInput("");
            }}
          >
            <div className="relative flex grow items-stretch focus-within:z-10">
              <label htmlFor="video-id" className="sr-only">
                Video ID
              </label>
              <input
                id="video-id"
                name="video-id"
                type="text"
                placeholder="Video ID"
                value={addToQueueInput}
                onChange={(e) => setAddToQueueInput(e.target.value)}
                className="block w-full rounded-none rounded-l-md border-0 py-1.5 px-3 text-gray-900 ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm/6"
              />
            </div>
            <button
              type="submit"
              className="relative -ml-px inline-flex items-center gap-x-1.5 rounded-r-md px-3 py-2 text-sm font-semibold text-gray-900 ring-1 ring-inset ring-gray-300 hover:bg-gray-50"
            >
              Add to Queue
            </button>
          </form>
          <p className="py-2 text-center border-b">Current queue:</p>
          <ul role="list" className="divide-y divide-gray-200">
            {queuedVideos.length > 0 ? (
              queuedVideos.map((video) => (
                <li key={video.videoID} className="py-4 text-ellipsis block overflow-hidden">
                  {handle(video.submitter) ? (
                    <>
                      {handle(video.submitter)}
                      {video.submitter === myClientID ? " (you)" : ""}
                    </>
                  ) : (
                    <code>ERROR: UNKNOWN SUBMITTER: {JSON.stringify(video.submitter)}</code>
                  )}
                  : {video.videoID}
                </li>
              ))
            ) : (
              <li className="py-4">No queued videos</li>
            )}
          </ul>
        </div>
        <div>
          {hasActiveVideo && (playerHostID !== myClientID || skipVotes > 0) && (
            <div className="mb-4">
              {playerHostID !== myClientID && (
                <button
                  type="submit"
                  className="mr-2 relative inline-flex items-center gap-x-1.5 rounded-md px-3 py-2 text-sm font-semibold text-gray-900 ring-1 ring-inset ring-gray-300 hover:bg-gray-50"
                  onClick={() => {
                    const nextVote = !votedToSkip;
                    setVotedToSkip(nextVote);
                    sendJsonMessage({
                      tag: "Vote",
                      skip: nextVote,
                    });
                  }}
                >
                  {votedToSkip ? "Unvote to Skip" : "Vote to Skip"}
                </button>
              )}
              {skipVotes > 0 && (
                <span>
                  Votes: {skipVotes}/{clients.length - 1} ({Math.ceil((clients.length - 1) / 2) + 1} needed)
                </span>
              )}
            </div>
          )}
          <p>Current listeners:</p>
          <ul className="list-disc list-inside ml-4">
            {clients.map((client) => (
              <li key={client.clientID}>
                {client.handle} {client.clientID === myClientID && "(you)"}
              </li>
            ))}
          </ul>
          <p className="mt-4">Change your handle:</p>
          <form
            className="mt-2 flex rounded-md shadow-sm"
            onSubmit={(e) => {
              e.preventDefault();
              sendJsonMessage({
                tag: "SetHandle",
                handle: newHandleInput,
              });
              setNewHandleInput("");
            }}
          >
            <div className="relative flex grow items-stretch focus-within:z-10">
              <label htmlFor="handle" className="sr-only">
                handle
              </label>
              <input
                id="handle"
                name="handle"
                type="text"
                placeholder="New handle"
                value={newHandleInput}
                onChange={(e) => setNewHandleInput(e.target.value)}
                className="block w-full rounded-none rounded-l-md border-0 py-1.5 px-3 text-gray-900 ring-1 ring-inset ring-gray-300 placeholder:text-gray-400 focus:ring-2 focus:ring-inset focus:ring-indigo-600 sm:text-sm/6"
              />
            </div>
            <button
              type="submit"
              className="relative -ml-px inline-flex items-center gap-x-1.5 rounded-r-md px-3 py-2 text-sm font-semibold text-gray-900 ring-1 ring-inset ring-gray-300 hover:bg-gray-50"
            >
              Update
            </button>
          </form>
        </div>
      </div>
    </div>
  );
}
