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
    resolve();
  };
});
let player = null;

// When control elements are swapped in via WebSocket, apply control actions to
// the player.
htmx.on('htmx:oobBeforeSwap', (e) => {
  const fragment = e.detail.fragment;
  if (fragment.getElementById('player-state')) {
    e.preventDefault();
    e.stopPropagation();
    console.log('Got player-state control element');

    YTReady.then(() => {
      console.log('YT API is ready');
      const data = fragment.getElementById('player-state').dataset;
      const playerElement = document.getElementById('player');
      console.log('Control element data:', data);

      if (player) {
        console.log('Queueing new video in existing player');
        player.cueVideoById({
          videoId: data.videoId,
        });
      } else {
        console.log('Constructing new player');
        player = new YT.Player('player', {
          width: playerElement.offsetWidth,
          videoId: data.videoId,
          playerVars: {
            playsinline: 1,
            enablejsapi: 1,
            disablekb: 1,
            controls: 0,
          },
          // Add event listeners to send player control events back through the
          // WebSocket.
          events: {
            onReady: (e) => console.log('Player ready', e),
            onStateChange: (e) => {
              console.log('Player state change', e);
              if (socket) {
                switch (e.data) {
                  case YT.PlayerState.PLAYING:
                    socket.send(JSON.stringify({ action: 'play' }));
                    break;
                  case YT.PlayerState.PAUSED:
                    socket.send(JSON.stringify({ action: 'pause' }));
                    break;
                  case YT.PlayerState.CUED:
                    socket.send(
                      JSON.stringify({
                        action: 'set-video-id',
                        'video-id': new URL(
                          player.getVideoUrl()
                        ).searchParams.get('v'),
                      })
                    );
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
    });
  }
  // debugger;
});
