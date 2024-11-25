import { StrictMode } from "react";
import ReactDOM from "react-dom/client";

import * as Sentry from "@sentry/react";
import { PostHogProvider } from "posthog-js/react";

import Room from "./Room";

Sentry.init({
  dsn: "https://0da9fc1284620f49e9c5372ab2707cb5@o4508357896634368.ingest.us.sentry.io/4508357905612800",
  integrations: [Sentry.browserTracingIntegration(), Sentry.replayIntegration()],
  // Tracing
  tracesSampleRate: 1.0, //  Capture 100% of the transactions
  // Set 'tracePropagationTargets' to control for which URLs distributed tracing should be enabled
  tracePropagationTargets: ["localhost", /^https:\/\/yourserver\.io\/api/],
  // Session Replay
  replaysSessionSampleRate: 0.1, // This sets the sample rate at 10%. You may want to change it to 100% while in development and then sample at a lower rate in production.
  replaysOnErrorSampleRate: 1.0, // If you're not already sampling the entire session, change the sample rate to 100% when sampling sessions where errors occur.
});

ReactDOM.createRoot(document.getElementById("react-root")!).render(
  <StrictMode>
    <PostHogProvider
      // @ts-expect-error
      apiKey={process.env.REACT_APP_PUBLIC_POSTHOG_KEY}
      options={{
        // @ts-expect-error
        api_host: process.env.REACT_APP_PUBLIC_POSTHOG_API_HOST,
        // @ts-expect-error
        ui_host: process.env.REACT_APP_PUBLIC_POSTHOG_UI_HOST,
      }}
    >
      <Room />
    </PostHogProvider>
  </StrictMode>,
);
