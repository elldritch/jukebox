// Enable event logging.
htmx.logger = (elt, event, data) => {
  console.log(event, elt, data);
};

// Hook into HTMX WebSocket events.
let socket = null;
htmx.on('htmx:wsOpen', (e) => {
  socket = e.detail.socketWrapper;
});
htmx.on('htmx:wsClose', (e) => {
  socket = null;
});

// Hook into YouTube API events.
const YTReady = new Promise((resolve) => {
  window.onYouTubeIframeAPIReady = () => {
    console.log('YT API is ready');
    resolve();
  };
});

// Set up player state.
let player = null;
let currentVideoID = null;
let currentPlaybackStatus = { state: 'stopped' };

// When control elements are swapped in via WebSocket, apply control actions to
// the player.
htmx.on('htmx:oobBeforeSwap', (e) => {
  // Parse the HTMX fragment for the player control element.
  const fragment = e.detail.fragment;
  const controlElement = fragment.getElementById('player-state');
  if (!controlElement) {
    return;
  }
  console.log('Got player-state control element');
  e.preventDefault();
  e.stopPropagation();

  // Parse the control element data.
  const data = controlElement.dataset;
  console.log('Control element raw data:', data);
  const videoId = data.videoId;
  const playbackStatus = { state: data.playbackStatus };
  if (playbackStatus.state === 'playing') {
    playbackStatus.started = data.playbackStarted;
  }
  console.log('Parsed control action:', { videoId, playbackStatus });
  console.log('Current control action:', {
    currentVideoID,
    currentPlaybackStatus,
  });

  // Apply the control action to the player.
  YTReady.then(() => {
    // Set the video ID if it needs to be changed.
    if (videoId !== currentVideoID) {
      console.log('Loading new video ID');
      currentVideoID = videoId;
      if (player) {
        console.log('Queueing new video in existing player');
        player.cueVideoById({
          videoId: data.videoId,
        });
      } else {
        console.log('Constructing new player');
        const playerElement = document.getElementById('player');
        player = new YT.Player('player', {
          width: playerElement.offsetWidth,
          videoId: data.videoId,
          playerVars: {
            playsinline: 1,
            enablejsapi: 1,
            disablekb: 1,
            controls: 1,
            autoplay: 0,
          },
          // Add event listeners to send player control events back through the
          // WebSocket.
          events: {
            onReady: (e) => console.log('Player ready', e),
            onAutoplayBlocked: (e) => console.log('Player autoplay blocked', e),
            onStateChange: (e) => {
              console.log('Player state change', e);
              if (socket) {
                switch (e.data) {
                  case YT.PlayerState.PLAYING:
                    if (currentPlaybackStatus.state !== 'playing') {
                      currentPlaybackStatus = {
                        state: 'playing',
                        started: new Date(),
                      };
                      // TODO: Also need to send current timestamp for seeking
                      // new clients.
                      socket.send(JSON.stringify({ action: 'play' }));
                    }
                    break;
                  case YT.PlayerState.PAUSED:
                    if (currentPlaybackStatus.state !== 'stopped') {
                      currentPlaybackStatus = { state: 'stopped' };
                      socket.send(JSON.stringify({ action: 'stop' }));
                    }
                    break;
                  case YT.PlayerState.CUED:
                    const playerVideoID = new URL(
                      player.getVideoUrl()
                    ).searchParams.get('v');
                    if (playerVideoID !== currentVideoID) {
                      currentVideoID = playerVideoID;
                      socket.send(
                        JSON.stringify({
                          action: 'set-video-id',
                          'video-id': playerVideoID,
                        })
                      );
                    }
                    break;
                  case YT.PlayerState.BUFFERING:
                  case YT.PlayerState.ENDED:
                  case YT.PlayerState.UNSTARTED:
                    // We do nothing for these events.
                    break;
                  default:
                    throw new Error('Unknown player state change type', e.data);
                }
              }
            },
          },
        });
      }
    }

    // Set the playback status if it needs to be changed.
    if (playbackStatus.state !== currentPlaybackStatus.state) {
      console.log(
        'Setting playback status from ',
        currentPlaybackStatus,
        ' to ',
        playbackStatus
      );
      currentPlaybackStatus = playbackStatus;
      if (playbackStatus.state === 'playing') {
        console.log('Playing video');
        // TODO: Add offset from playbackStarted to current time.

        // FIXME: The autoplay gets blocked here. Need to figure out a way
        // around this.
        player.playVideo();
      } else if (playbackStatus.state === 'stopped') {
        console.log('Stopping video');
        player.pauseVideo();
      }
    }
  });
});
