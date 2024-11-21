import { useEffect, useId, useRef, useState } from "react";

import { debug, debugging } from "./debug";

export type PlayerController = {
  seekTo(seconds: number, allowSeekAhead: true): void;
  getDuration(): number;
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
  useEffect(() => {
    let loaded = false;
    let iframeAPIReady = false;
    function onYTReady() {
      debug("onYTReady: constructing player");
      playerRef.current = new (window as any).YT.Player(playerId, {
        width: "100%",
        playerVars: {
          playsinline: 1,
          enablejsapi: 1,
          disablekb: 1,
          controls: 0,
          autoplay: 0,
        },
        events: {
          onReady(e: { target: PlayerController }) {
            // Sometimes this player just straight up never loads, for reasons I
            // also do not understand. This timeout and hard refresh helps
            // mitigate. For some reason, this seems to happen more when I have
            // more simultaneous tabs open (for testing). Maybe it's a network
            // thing?
            const failureTimeout = setTimeout(() => {
              window.location.reload();
            }, 5000);

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
              const keys = Object.keys(playerRef.current);
              debug("onReady: keys", { keys });
              if (keys.includes("loadVideoById")) {
                clearTimeout(failureTimeout);
                clearInterval(interval);

                const videoId = propsRef.current?.["videoId"];
                debug("onReady: videoId", { videoId });
                if (videoId) {
                  const playing = propsRef.current?.["playing"];
                  debug("onReady: playing", { playing });
                  if (playing) {
                    debug("onReady: loadVideoById");
                    playerRef.current.loadVideoById(videoId);
                  } else {
                    debug("onReady: cueVideoById");
                    playerRef.current.cueVideoById(videoId);
                  }
                  setLastVideoId(videoId);
                }
                const onReady = propsRef.current?.["onReady"];
                debug("onReady: onReady", { onReady });
                onReady?.(e);
                setPlayerReady(true);
              }
            }, 500);
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
            propsRef.current?.["onError"]?.(err);
          },
        },
      });
      if (debugging()) {
        debug("onYTReady: setting window.player");
        (window as any).player = playerRef.current;
      }
    }
    window.addEventListener("load", () => {
      debug("window.addEventListener: load");
      loaded = true;
      if (iframeAPIReady) {
        onYTReady();
      }
    });
    // TODO: Add proper typings for these globals and externals.
    (window as any).onYouTubeIframeAPIReady = () => {
      debug("window.onYouTubeIframeAPIReady");
      iframeAPIReady = true;
      if (loaded) {
        onYTReady();
      }
    };
    if ((window as any).YT?.loaded === 1) {
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
    debug("useEffect: [playerReady, volume]", { playerReady, volume });
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
