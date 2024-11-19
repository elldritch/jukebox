import { useState, useEffect } from 'react';

import useWebSocket, { ReadyState } from 'react-use-websocket';
import ReactPlayer from 'react-player';

type ClientID = string;

type Client = {
  handle: string;
  clientID: ClientID;
};

type UpdateClientList = {
  tag: 'UpdateClientList';
  clients: Client[];
  you: Client;
};

type ServerMessage = UpdateClientList;

export default function Room() {
  // Connect to WebSocket.
  const { sendJsonMessage, lastJsonMessage, readyState } =
    useWebSocket<ServerMessage>(document.location.pathname);

  useEffect(() => {
    if (lastJsonMessage !== null) {
      const msg = lastJsonMessage;
      // Handle messages. The `useEffect` helps deduplicate `lastJsonMessage`s.
      console.log(msg);

      switch (msg.tag) {
        case 'UpdateClientList': {
          setClients(msg.clients);
          setMyClientID(msg.you.clientID);
          break;
        }
        default: {
          const _exhaustiveCheck: never = msg.tag;
          console.error('Unknown message', msg);
        }
      }
    }
  }, [lastJsonMessage]);

  // Hooks related to the client list.
  const [clients, setClients] = useState<Client[]>([]);
  const [myClientID, setMyClientID] = useState('');
  const [newHandleInput, setNewHandleInput] = useState('');

  // The video URL sent to the player.
  const [playerURL, setPlayerURL] = useState('');
  // The URL in the controlled form input of the "Add to Queue" element.
  const [playerURLInput, setPlayerURLInput] = useState('');

  // The desired `playing` status. We maintain this to quickly override user
  // inputs in the player using `setTimeout(..., 1)` when needed.
  const [playingDesired, setPlayingDesired] = useState(false);
  // The actual `playing` status reflecting the player's state. Note that this
  // can go out of sync with the player if the player callbacks are not
  // respected. The player only synchronizes with this value _when the value
  // changes_.
  const [playingActual, setPlayingActual] = useState(false);

  // Render disconnected state. Note that all returns must occur after all hooks
  // are called, so that all hooks are called in the same order every render.
  const connectionStatus = {
    [ReadyState.CONNECTING]: 'connecting',
    [ReadyState.OPEN]: 'open',
    [ReadyState.CLOSING]: 'closing',
    [ReadyState.CLOSED]: 'closed',
    [ReadyState.UNINSTANTIATED]: 'uninstantiated',
  }[readyState];
  if (readyState !== ReadyState.OPEN) {
    return (
      <div className="mx-auto max-w-lg mt-8">
        One moment, connecting to server. Current status:{' '}
        <code>{connectionStatus}</code>
      </div>
    );
  }

  // Render happy path.
  return (
    <>
      <div className="mx-auto max-w-2xl mt-8">
        <ReactPlayer
          url={playerURL}
          playing={playingActual}
          onPause={() => {
            setPlayingActual(false);
            setTimeout(() => setPlayingActual(true), 1);
          }}
        />
        <button onClick={() => setPlayingActual(!playingActual)}>Play</button>
        <div>
          <p>Now playing: title, link, creator, createdAt, view count</p>
          <p>Presented by: submitter</p>
        </div>
      </div>
      <div className="mx-auto max-w-2xl mt-8 grid grid-cols-2 gap-x-4">
        <div>
          <form
            className="flex rounded-md shadow-sm"
            onSubmit={(e) => {
              console.log(e);
              e.preventDefault();
              setPlayerURL(playerURLInput);
              setPlayerURLInput('');
              setTimeout(() => setPlayingActual(true), 2000);
            }}
          >
            <div className="relative flex grow items-stretch focus-within:z-10">
              <label htmlFor="video-url" className="sr-only">
                Video URL
              </label>
              <input
                id="video-url"
                name="video-url"
                type="text"
                placeholder="Video URL"
                value={playerURLInput}
                onChange={(e) => setPlayerURLInput(e.target.value)}
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
          <p className="mt-4">Next up:</p>
          <ul role="list" className="divide-y divide-gray-200">
            <li className="py-4">Link by Submitter</li>
            <li className="py-4">Link by Submitter</li>
            <li className="py-4">Link by Submitter</li>
          </ul>
        </div>
        <div>
          Upvote/Downvote
          <p className="mt-4">Current listeners:</p>
          <ul className="list-disc list-inside ml-4">
            {clients.map((client) => (
              <li key={client.clientID}>
                {client.handle} {client.clientID === myClientID && '(you)'}
              </li>
            ))}
          </ul>
          <p className="mt-4">Change your handle:</p>
          <form
            className="mt-2 flex rounded-md shadow-sm"
            onSubmit={(e) => {
              e.preventDefault();
              sendJsonMessage({
                tag: 'SetHandle',
                handle: newHandleInput,
              });
              setNewHandleInput('');
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
    </>
  );
}
