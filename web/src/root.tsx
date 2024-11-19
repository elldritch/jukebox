import { StrictMode } from 'react';
import ReactDOM from 'react-dom/client';

import Room from './Room';

ReactDOM.createRoot(document.getElementById('react-root')!).render(
  <StrictMode>
    <Room />
  </StrictMode>
);
