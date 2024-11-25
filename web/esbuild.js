import * as esbuild from "esbuild";
import { sentryEsbuildPlugin } from "@sentry/esbuild-plugin";

const processDefines = {};
for (const [key, value] of Object.entries(process.env)) {
  processDefines[`process.env.${key}`] = JSON.stringify(value);
}

const plugins = [];
if (process.env.NODE_ENV === "production") {
  // Put the Sentry esbuild plugin after all other plugins
  plugins.push(
    sentryEsbuildPlugin({
      authToken: process.env.SENTRY_AUTH_TOKEN,
      org: process.env.SENTRY_ORG,
      project: process.env.SENTRY_PROJECT,
    }),
  );
}

const ctx = await esbuild.context({
  entryPoints: ["./src/root.tsx"],
  bundle: true,
  outfile: "../static/room.js",
  sourcemap: true, // Source map generation must be turned on for Sentry
  minify: true,
  plugins,
  define: { ...processDefines },
});

if (process.env.NODE_ENV === "production") {
  await ctx.rebuild();
} else {
  await ctx.watch();
}
