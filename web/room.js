function onYouTubeIframeAPIReady() {
  const playerElement = document.getElementById('player');
  const width = playerElement.offsetWidth;

  const player = new YT.Player('player', {
    width,
    videoId: 'M7lc1UVf-VE',
    playerVars: {
      playsinline: 1,
      enablejsapi: 1,
      disablekb: 1,
      controls: 0,
    },
    events: {
      onReady: (e) => console.log('ready!', e),
      onStateChange: (e) => console.log('state change!', e),
    },
  });
}
