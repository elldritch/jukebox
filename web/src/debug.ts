// TODO: Change `ALWAYS_DEBUG` value.
// @ts-expect-error
const ALWAYS_DEBUG = true || process.env.NODE_ENV === "development";

export function debugging(): boolean {
  return ALWAYS_DEBUG || (window as any)["jukeboxRuntimeDebug"] === true;
}

export function debug(...args: any[]) {
  if (debugging()) {
    console.log(...args);
  }
}
