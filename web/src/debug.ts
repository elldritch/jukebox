// @ts-expect-error
const ALWAYS_DEBUG = process.env.NODE_ENV === "development";

export function debugging(): boolean {
  return ALWAYS_DEBUG || (window as any)["jukeboxRuntimeDebug"] === true;
}

export function debug(...args: any[]) {
  if (debugging()) {
    console.log(...args);
  }
}
