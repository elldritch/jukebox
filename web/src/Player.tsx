import { useEffect, useId, useRef, useState } from "react";

// TODO: Turn this off in production.
// @ts-expect-error
const ALWAYS_DEBUG = process.env.NODE_ENV === "development";
const debugging = () => ALWAYS_DEBUG || (window as any)["jukeboxRuntimeDebug"] === true;
function debug(...args: any[]) {
  if (debugging()) {
    console.log(...args);
  }
}

export type PlayerController = {
  seekTo(seconds: number, allowSeekAhead: true): void;
  getDuration(): number;
};

export type PlayerProps = {
  videoId: string;
  playing: boolean;
  width?: String;
  volume?: number;
  muted?: boolean;
  onReady?(e: { target: PlayerController }): void;
  onPlay?(): void;
  onPause?(): void;
  onProgress?(e: { playedSeconds: number }): void;
  onEnded?(): void;
  onAutoplayBlocked?(): void;
  onError?(e: { data: number }): void;
};

export default function Player(props: PlayerProps) {
  const {
    videoId,
    playing,
    width,
    volume,
    muted,
    onReady,
    onPlay,
    onPause,
    onProgress,
    onEnded,
    onAutoplayBlocked,
    onError,
  } = props;

  const playerId = useId();
  const playerRef = useRef<any>(null);
  const [playerReady, setPlayerReady] = useState(false);

  // This insane hack uses a Ref in order to store a map to props that are used
  // in the player's event handlers. This is required because the event handlers
  // are set on construction, and only run exactly once, which means they only
  // ever close over the first snapshot of prop values. If we don't use a ref
  // here, the prop values (e.g. `videoId`) referenced in the event handlers
  // will always be their _initial_ values, not their latest values. This map is
  // an indirection trick so that the handlers can reference the latest values.
  const propsRef = useRef<Record<string, any>>({});
  propsRef.current["videoId"] = videoId;
  propsRef.current["playing"] = playing;
  propsRef.current["onReady"] = onReady;
  propsRef.current["onEnded"] = onEnded;
  propsRef.current["onPlay"] = onPlay;
  propsRef.current["onPause"] = onPause;
  propsRef.current["onAutoplayBlocked"] = onAutoplayBlocked;
  propsRef.current["onError"] = onError;

  // State tracking for old props where setting has side effects.
  const [lastVideoId, setLastVideoId] = useState("");

  // Initialize the player.
  useEffect(() => {
    let loaded = false;
    let iframeAPIReady = false;
    function onYTReady() {
      playerRef.current = new (window as any).YT.Player(playerId, {
        width,
        playerVars: {
          playsinline: 1,
          enablejsapi: 1,
          disablekb: 1,
          controls: 0,
          autoplay: 0,
        },
        events: {
          onReady(e: { target: PlayerController }) {
            // I know this looks insane but I promise it's needed, because some
            // of the methods on the player (like `loadVideoById`) don't exist
            // after construction. Apparently, they're added asynchronously.
            //
            // You can observe this behavior with:
            //
            // > setInterval(() => console.log(Object.keys(playerRef.current)), 100);
            //
            // on the player immediately after construction, and watch as the
            // object's keys change. It is truly baffling. I spent so fucking
            // long debugging this.
            const interval = setInterval(() => {
              if (Object.keys(playerRef.current).includes("loadVideoById")) {
                clearInterval(interval);

                if (propsRef.current?.["videoId"]) {
                  if (propsRef.current?.["playing"]) {
                    console.log("load vid");
                    playerRef.current.loadVideoById(videoId);
                  } else {
                    console.log("cue vid");
                    playerRef.current.cueVideoById(videoId);
                  }
                  setLastVideoId(videoId);
                }
                propsRef.current?.["onReady"]?.(e);
                setPlayerReady(true);
              }
            }, 1000);
          },
          onStateChange(e: { data: number }) {
            if (e.data === 0) {
              propsRef.current?.["onEnded"]?.();
            } else if (e.data === 1) {
              propsRef.current?.["onPlay"]?.();
            } else if (e.data === 2) {
              propsRef.current?.["onPause"]?.();
            }
          },
          onAutoplayBlocked() {
            propsRef.current?.["onAutoplayBlocked"]?.();
          },
          onError(e: { data: number }) {
            propsRef.current?.["onError"]?.(e);
          },
        },
      });
      if (debugging()) {
        (window as any).player = playerRef.current;
      }
    }
    window.addEventListener("load", () => {
      debug("load");
      loaded = true;
      if (iframeAPIReady) {
        onYTReady();
      }
    });
    (window as any).onYouTubeIframeAPIReady = () => {
      debug("onYouTubeIframeAPIReady");
      iframeAPIReady = true;
      if (loaded) {
        onYTReady();
      }
    };
    if ((window as any).YT.loaded === 1) {
      (window as any).onYouTubeIframeAPIReady();
    }
  }, []);

  // Synchronize player state with various props.
  useEffect(() => {
    debug("useEffect: [playerReady, videoId, lastVideoId]", { playerReady, videoId, lastVideoId });
    if (playerReady && playerRef.current && videoId !== lastVideoId) {
      if (playing) {
        debug("Called loadVideoById");
        playerRef.current.loadVideoById(videoId);
      } else {
        debug("Called cueVideoById");
        playerRef.current.cueVideoById(videoId);
      }
      setLastVideoId(videoId);
    }
  }, [playerReady, videoId, lastVideoId]);
  useEffect(() => {
    debug("useEffect: [playerReady, playing]", { playerReady, playing });
    if (playerReady && playerRef.current) {
      if (playing) {
        debug("Called playVideo");
        playerRef.current.playVideo();
      } else {
        debug("Called pauseVideo");
        playerRef.current.pauseVideo();
      }
    }
  }, [playerReady, playing]);
  useEffect(() => {
    if (playerReady && playerRef.current) {
      playerRef.current.setVolume(volume);
    }
  }, [playerReady, volume]);
  useEffect(() => {
    if (playerReady && playerRef.current) {
      if (muted) {
        playerRef.current.mute();
      } else {
        playerRef.current.unMute();
      }
    }
  }, [playerReady, muted]);
  useEffect(() => {
    const interval = setInterval(() => {
      if (playerReady && playerRef.current && playerRef.current.getPlayerState() === 1) {
        onProgress?.({ playedSeconds: playerRef.current.getCurrentTime() });
      }
    }, 100);
    return () => clearInterval(interval);
  }, [onProgress]);

  return (
    <div>
      <div id={playerId}></div>
    </div>
  );
}
