import * as esbuild from "esbuild";
import { sentryEsbuildPlugin } from "@sentry/esbuild-plugin";

await esbuild.build({
  entryPoints: ["./src/root.tsx"],
  bundle: true,
  outfile: "../static/room.js",
  sourcemap: true, // Source map generation must be turned on for Sentry
  minify: true,
  plugins: [
    // Put the Sentry esbuild plugin after all other plugins
    sentryEsbuildPlugin({
      authToken: process.env.SENTRY_AUTH_TOKEN,
      org: "jukebox-casa",
      project: "frontend",
    }),
  ],
});
