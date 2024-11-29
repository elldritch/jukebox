import { useEffect, useId, useRef, useState } from "react";

export type PlayerController = {
  seekTo(seconds: number, allowSeekAhead: true): void;
  getDuration(): number | undefined;
};

export type PlayerProps = {
  videoId: string;
  playing: boolean;
  volume?: number;
  muted?: boolean;
  onReady?(e: { target: PlayerController }): void;
  onPlay?(): void;
  onPause?(): void;
  onProgress?(e: { playedSeconds: number }): void;
  onEnded?(): void;
  onAutoplayBlocked?(): void;
  onError?(e: { code: number; message: string }): void;
};

export default function Player(props: PlayerProps) {
  const { videoId, playing, volume, muted, onReady, onPlay, onPause, onProgress, onEnded, onAutoplayBlocked, onError } =
    props;

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
  const loadedRef = useRef(false);
  const iframeAPIReadyRef = useRef(false);
  useEffect(() => {
    function onYTReady() {
      // This is needed because React's StrictMode runs useEffect twice, but
      // there's no way to destroy the side effect of player construction
      // because the player initializes asynchronously and the `destroy` method
      // is not available by the time the destructor is called.
      if (playerRef.current) {
        return;
      }
      console.log("onYTReady: constructing player");
      playerRef.current = new (window as any).YT.Player(playerId, {
        width: "100%",
        playerVars: {
          playsinline: 1,
          enablejsapi: 1,
          disablekb: 1,
          controls: 0,
          autoplay: 1,
        },
        events: {
          onReady(e: { target: PlayerController }) {
            const videoId = propsRef.current?.["videoId"];
            if (videoId) {
              console.log("onReady: videoId", { videoId });
              if (propsRef.current?.["playing"]) {
                playerRef.current.loadVideoById(videoId);
              } else {
                playerRef.current.cueVideoById(videoId);
              }
              setLastVideoId(videoId);
            }
            propsRef.current?.["onReady"]?.(e);
            setPlayerReady(true);
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
          onError(e: { data: number; target: PlayerController }) {
            let message = "UNKNOWN_ERROR";
            switch (e.data) {
              case 2:
                message = "INVALID_PARAMETER_VALUE";
                break;
              case 5:
                message = "HTML5_PLAYER_ERROR";
                break;
              case 100:
                message = "VIDEO_NOT_FOUND";
                break;
              case 101:
                message = "VIDEO_DOES_NOT_ALLOW_EMBEDDED_PLAYERS";
                break;
              case 150:
                message = "VIDEO_DOES_NOT_ALLOW_EMBEDDED_PLAYERS";
                break;
            }
            const err = { code: e.data, message };
            console.error(err);
            console.error(propsRef.current);
            propsRef.current?.["onError"]?.(err);
          },
        },
      });
      (window as any).player = playerRef.current;
    }
    if (window.document.readyState === "complete") {
      console.log("Document already loaded");
      loadedRef.current = true;
    } else {
      console.log("Setting event listener for load");
      window.addEventListener("load", () => {
        const iframeAPIReady = iframeAPIReadyRef.current;
        console.log("window.addEventListener: load", { iframeAPIReady });
        loadedRef.current = true;
        if (iframeAPIReady) {
          onYTReady();
        }
      });
    }
    // TODO: Add proper typings for these globals and externals.
    (window as any).onYouTubeIframeAPIReady = () => {
      const loaded = loadedRef.current;
      console.log("window.onYouTubeIframeAPIReady", { loaded });
      iframeAPIReadyRef.current = true;
      if (loaded) {
        onYTReady();
      }
    };
    if ((window as any).YT?.loaded === 1) {
      console.log("window.YT.loaded === 1");
      (window as any).onYouTubeIframeAPIReady();
    }
  }, []);

  // Synchronize player state with various props.
  useEffect(() => {
    console.log("useEffect: [playerReady, videoId, lastVideoId]", { playerReady, videoId, lastVideoId });
    if (playerReady && playerRef.current && videoId !== lastVideoId) {
      if (playing) {
        console.log("Called loadVideoById");
        playerRef.current.loadVideoById(videoId);
      } else {
        console.log("Called cueVideoById");
        playerRef.current.cueVideoById(videoId);
      }
      setLastVideoId(videoId);
    }
  }, [playerReady, videoId, lastVideoId]);
  useEffect(() => {
    console.log("useEffect: [playerReady, playing]", { playerReady, playing });
    if (playerReady && playerRef.current) {
      if (playing) {
        console.log("Called playVideo");
        playerRef.current.playVideo();
      } else {
        console.log("Called pauseVideo");
        playerRef.current.pauseVideo();
      }
    }
  }, [playerReady, playing]);
  useEffect(() => {
    console.log("useEffect: [playerReady, volume]", { playerReady, volume });
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
