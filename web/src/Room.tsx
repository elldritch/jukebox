import { useState, useEffect, useRef } from "react";

import useWebSocket, { ReadyState } from "react-use-websocket";
import ReactPlayer from "react-player";
import formatDuration from "format-duration";

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
  videoURL: string;
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
  videoURL: string;
  submitter: ClientID;
};

type ServerMessage = UpdateClientList | UpdateQueue | SetPlayer | UnsetPlayer;

const SECONDS_DRIFT_ALLOWED = 2;

// TODO: Turn this off in production.
// @ts-expect-error
const ALWAYS_DEBUG = process.env.NODE_ENV === "development";

function debug(...args: any[]) {
  if (ALWAYS_DEBUG || (window as any)["jukeboxRuntimeDebug"] === true) {
    console.log(...args);
  }
}

export default function Room() {
  // Connect to WebSocket.
  const { sendJsonMessage, lastJsonMessage, readyState, getWebSocket } = useWebSocket<ServerMessage>(
    document.location.pathname
  );

  useEffect(() => {
    if (lastJsonMessage !== null) {
      const msg = lastJsonMessage;
      // Handle messages. The `useEffect` helps deduplicate `lastJsonMessage`s.
      debug(msg);

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
        case "SetPlayer": {
          setHasActiveVideo(true);
          setPlayerURL(msg.videoURL);
          setPlayerHostID(msg.submitter);
          switch (msg.playbackStatus.tag) {
            case "Playing": {
              // Calculate correct seek, compensating for start time.
              const started = new Date(msg.playbackStatus.started);
              const now = new Date();
              const elapsedSeconds = Math.max(0, (now.getTime() - started.getTime()) / 1000);
              const seekTarget = elapsedSeconds + msg.playbackStatus.fromSeekSeconds;

              // Allow clients to drift a tiny little bit. Otherwise they will
              // jump a bit to try and snap to the "correct" seek.
              if (Math.abs(seekTarget - activeVideoPlayedSeconds) > SECONDS_DRIFT_ALLOWED) {
                playerRef.current?.seekTo(seekTarget, "seconds");
                setActiveVideoPlayedSeconds(seekTarget);
              }

              setPlayingDesired(true);
              if (playerReady) {
                setPlayingActual(true);
              }
              break;
            }
            case "Paused": {
              // Allow clients to drift a tiny little bit. Otherwise they will
              // jump a bit on pause to try and snap to the "correct" seek.
              if (Math.abs(msg.playbackStatus.atSeekSeconds - activeVideoPlayedSeconds) > SECONDS_DRIFT_ALLOWED) {
                playerRef.current?.seekTo(msg.playbackStatus.atSeekSeconds, "seconds");
                setActiveVideoPlayedSeconds(msg.playbackStatus.atSeekSeconds);
              }

              setPlayingDesired(false);
              if (playerReady) {
                setPlayingActual(false);
              }
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
          setHasActiveVideo(false);
          setPlayerURL("");
          setPlayerHostID("");
          setActiveVideoPlayedSeconds(0);
          setActiveVideoDuration(undefined);
          setPlayingDesired(false);
          if (playerReady) {
            setPlayingActual(false);
          }
          break;
        }
        default: {
          const _exhaustiveCheck: never = msg;
          console.error("Unknown message", _exhaustiveCheck);
        }
      }
    }
  }, [lastJsonMessage]);

  // Hooks related to the client list.
  const [clients, setClients] = useState<Client[]>([]);
  const [myClientID, setMyClientID] = useState("");
  const [newHandleInput, setNewHandleInput] = useState("");

  // Hooks related to player health.
  const playerRef = useRef<ReactPlayer>(null);
  const [playerReady, setPlayerReady] = useState(false);
  const [playerError, setPlayerError] = useState<{} | undefined>(undefined);

  // Hooks related to playback.
  const [showAutoplayBlockedDialog, setShowAutoplayBlockedDialog] = useState(false);
  const [playerVolume, setPlayerVolume] = useState<number | null>(null);
  const [playerMuted, setPlayerMuted] = useState(false);
  // The desired `playing` status. We maintain this to quickly override user
  // inputs in the player using `setTimeout(..., 1)` when needed.
  const [playingDesired, setPlayingDesired] = useState(false);
  // The actual `playing` status reflecting the player's state. Note that this
  // can go out of sync with the player if the player callbacks are not
  // respected. The player only synchronizes with this value _when the value
  // changes_.
  const [playingActual, setPlayingActual] = useState(false);

  // Hooks related to the active video.
  const [hasActiveVideo, setHasActiveVideo] = useState(false);
  const [playerURL, setPlayerURL] = useState("");
  const [playerHostID, setPlayerHostID] = useState("");
  const [activeVideoDuration, setActiveVideoDuration] = useState<number | undefined>(undefined);
  const [activeVideoPlayedSeconds, setActiveVideoPlayedSeconds] = useState(0);

  // Hooks related to the player seeking control.
  const [isSeeking, setIsSeeking] = useState(false);
  const [seekTarget, setSeekTarget] = useState(0);

  // Hooks related to the queue.
  const [addToQueueInput, setAddToQueueInput] = useState("");
  const [queuedVideos, setQueuedVideos] = useState<QueuedVideo[]>([]);

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
  if (playerError) {
    console.error(playerError);
    return (
      <div className="mx-auto max-w-lg mt-8 text-red-700 border-l-4 border-red-400 bg-red-50 p-4">
        <p>
          Whoa! Something broke.{" "}
          <a href="" className="font-medium underline">
            Try reloading?
          </a>
        </p>
        <p className="mt-2">
          Here's the error: <code>{JSON.stringify(playerError)}</code>
        </p>
      </div>
    );
  }

  // Render happy path.
  const handle = (clientID: ClientID) => clients.find((c) => c.clientID === clientID)?.handle;
  const formatSeconds = (seconds: number) => formatDuration(seconds * 1000);

  return (
    <div className="mx-auto max-w-2xl mt-8">
      <ReactPlayer
        ref={playerRef}
        url={playerURL}
        playing={playingActual}
        width="100%"
        playsinline
        // The ReactPlayer type declaration for `volume` is wrong.
        volume={playerVolume as number | undefined}
        muted={playerMuted}
        onReady={(e) => {
          debug("ready", e);
          setPlayerReady(true);
          if (playingDesired) {
            setPlayingActual(true);
          }
          // onDuration is just wrong sometimes for reasons that puzzle me.
          const duration = playerRef.current?.getDuration();
          if (duration) {
            setActiveVideoDuration(duration);
            // This fixes a UI glitch that can occur when the user joins a video
            // that was played to completion but has been over for a long time,
            // and the "video played seconds" is momentarily calculated to be
            // longer than the video duration.
            if (activeVideoPlayedSeconds > duration) {
              setActiveVideoPlayedSeconds(duration);
            }
          }
        }}
        onStart={() => {
          debug("start");
          sendJsonMessage({
            tag: "PlaybackStarted",
          });
        }}
        onPlay={() => {
          debug("play");
          setPlayingActual(true);
          if (!playingDesired) {
            setTimeout(() => setPlayingActual(false), 1);
          }
          sendJsonMessage({
            tag: "PlaybackStarted",
          });
        }}
        // We don't use onSeek because this event does not fire for YouTube:
        // https://github.com/cookpete/react-player/issues/1243
        onProgress={({ playedSeconds }) => {
          // debug("progress", playedSeconds);
          setActiveVideoPlayedSeconds(playedSeconds);

          // onDuration is just wrong sometimes for reasons that puzzle me.
          const duration = playerRef.current?.getDuration();
          if (duration) {
            setActiveVideoDuration(duration);
            // This fixes a UI glitch that can occur when the user joins a video
            // that was played to completion but has been over for a long time,
            // and the "video played seconds" is momentarily calculated to be
            // longer than the video duration.
            if (activeVideoPlayedSeconds > duration) {
              setActiveVideoPlayedSeconds(duration);
            }
          }
        }}
        onEnded={() => {
          debug("ended");
          if (activeVideoDuration) {
            setActiveVideoPlayedSeconds(activeVideoDuration);
          }
          sendJsonMessage({
            tag: "PlaybackFinished",
          });
        }}
        onError={(err) => {
          console.error(err);
          setPlayerError(err);
        }}
        onPause={() => {
          debug("pause");
          // Synchronize `playingActual` with the actual player state.
          setPlayingActual(false);

          // If the player _shouldn't_ be paused, immediately unpause it on
          // the next tick.
          if (playingDesired) {
            setTimeout(() => setPlayingActual(true), 1);
          }
        }}
        config={{
          youtube: {
            playerVars: {
              disablekb: 1,
            },
            onUnstarted: () => {
              // If this site does not have autoplay permissions for this user,
              // begin autoplay on mute.
              //
              // TODO: Is there a way to detect this for other players?
              debug("unstarted");
              setShowAutoplayBlockedDialog(true);
              setPlayingActual(false);
              setPlayerMuted(true);
              setTimeout(() => {
                playerRef.current?.seekTo(activeVideoPlayedSeconds, "seconds");
                setPlayingActual(true);
              }, 1);
            },
          },
        }}

        // TODO: Add callbacks for onBuffer and onBufferEnd and use them for
        // synchronization after buffering.
      />
      <div className="mt-4">
        {/* TODO: Load video information from API (title, channel, etc.) and display here. */}
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
                      setPlayingDesired(newPlayingDesired);
                      if (playerReady && newPlayingDesired !== playingActual) {
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
                  value={isSeeking ? seekTarget : activeVideoPlayedSeconds}
                  max={activeVideoDuration || 1}
                  step="any"
                  disabled={!(hasActiveVideo && playerReady && activeVideoDuration)}
                  onMouseDown={() => {
                    setIsSeeking(true);
                  }}
                  onChange={(e) => {
                    setSeekTarget(Number(e.target.value));
                  }}
                  onMouseUp={() => {
                    playerRef.current?.seekTo(seekTarget);
                    setActiveVideoPlayedSeconds(seekTarget);
                    setIsSeeking(false);
                    if (playingDesired) {
                      sendJsonMessage({
                        tag: "RequestPlay",
                        fromSeekSeconds: Math.round(seekTarget),
                      });
                    } else {
                      sendJsonMessage({
                        tag: "RequestPause",
                        atSeekSeconds: Math.round(seekTarget),
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
                onClick={() => {
                  if (playerMuted && showAutoplayBlockedDialog) {
                    setShowAutoplayBlockedDialog(false);
                  }
                  setPlayerMuted(!playerMuted);
                }}
              >
                {playerMuted ? "Unmute" : "Mute"}
              </button>
            </div>
            <span className="ml-2 text-sm">Volume</span>
            <input
              className="ml-2 inline-block align-middle"
              type="range"
              min="0"
              max="1"
              step="any"
              value={playerVolume || 1}
              onChange={(e) => setPlayerVolume(Number(e.target.value))}
              disabled={!playerReady}
            />
            <span className="ml-2 text-sm">{playerVolume ? Math.round(playerVolume * 100) + "%" : ""}</span>
          </div>
          {showAutoplayBlockedDialog && (
            <div className="text-sm text-red-700 border-l-4 border-red-400 bg-red-50 p-4 mt-2">
              <a href="https://developer.chrome.com/blog/autoplay/" target="_blank" className="font-medium underline">
                Chrome blocks autoplaying with sound
              </a>
              , so you may need to unmute or click the video to start playing.
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
              sendJsonMessage({
                tag: "AddToQueue",
                videoURL: addToQueueInput,
              });
              setAddToQueueInput("");
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
                <li key={video.videoURL} className="py-4 text-ellipsis block overflow-hidden">
                  {handle(video.submitter) ? (
                    <>
                      {handle(video.submitter)}
                      {video.submitter === myClientID ? " (you)" : ""}
                    </>
                  ) : (
                    <code>ERROR: UNKNOWN SUBMITTER: {JSON.stringify(video.submitter)}</code>
                  )}
                  : {video.videoURL}
                </li>
              ))
            ) : (
              <li className="py-4">No queued videos</li>
            )}
          </ul>
        </div>
        <div>
          {/* TODO: Implement voting */}
          Upvote/Downvote (unimplemented)
          <p className="mt-4">Current listeners:</p>
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
