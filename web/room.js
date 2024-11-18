// // TODO: Add a hack to prevent HTMX from running this script twice.

// // Set up callback for YouTube IFrame API and prepare player construction.
// const playerElement = document.getElementById('player');
// const width = playerElement.offsetWidth;
// const YTReady = new Promise((resolve) => {
//   window.onYouTubeIframeAPIReady = () => {
//     resolve();
//   };
// });

// // Connect to WebSocket.
// const ws = new WebSocket(document.location.pathname);

// // Set up WebSocket message handlers.
// ws.addEventListener('message', (e) => {
//   console.log('Received WebSocket message', e);
//   const data = JSON.parse(e.data);
//   console.log('Parsed WebSocket message data', data);

//   switch (data.tag) {
//     case 'ConnectedClientList':
//       const { clients, owner, you } = data;
//       break;
//     default:
//       console.log('Unrecognized WebSocket message', data);
//   }
// });

// YTReady.then(() => {
//   const playerElement = document.getElementById('player');
//   const width = playerElement.offsetWidth;

//   const player = new YT.Player('player', {
//     width,
//     // videoId: 'M7lc1UVf-VE',
//     playerVars: {
//       playsinline: 1,
//       enablejsapi: 1,
//       disablekb: 1,
//       controls: 0,
//     },
//     events: {
//       onReady: (e) => console.log('ready!', e),
//       onStateChange: (e) => console.log('state change!', e),
//     },
//   });
// });
