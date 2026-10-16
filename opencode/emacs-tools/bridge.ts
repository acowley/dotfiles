import { execFile } from "node:child_process";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve, sep } from "node:path";
import { promisify } from "node:util";

const run = promisify(execFile);

export function elispString(value: string): string {
  return `"${value.replace(/\\/g, "\\\\").replace(/"/g, '\\"').replace(/\n/g, "\\n").replace(/\r/g, "\\r").replace(/\t/g, "\\t")}"`;
}

export type ElispArgument = string | number | null | undefined | readonly string[];

function elispArgument(value: ElispArgument): string {
  if (typeof value === "string") return elispString(value);
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) throw new Error("Expected a safe integer");
    return String(value);
  }
  if (value === null || value === undefined) return "nil";
  return `'(${value.map(elispString).join(" ")})`;
}

export interface BridgeOptions {
  elispPath: string;
  cursorContextPath?: string;
  emacsclient?: string;
  timeout?: number;
}

/** Use argv, not a shell, and a private file rather than parsing printed Lisp. */
export async function emacsEval(
  options: BridgeOptions,
  cwd: string,
  fn: string,
  args: readonly ElispArgument[],
  signal?: AbortSignal,
): Promise<string> {
  // Only the plugin's fixed functions may be called; never interpolate Lisp input.
  if (!/^omp-[a-z-]+$/.test(fn) && fn !== "opencode-emacs-cursor-context") {
    throw new Error("Invalid Emacs tool function");
  }
  if (fn === "opencode-emacs-cursor-context" && !options.cursorContextPath) {
    throw new Error("Cursor context helper path is not configured");
  }
  signal?.throwIfAborted();
  const call = `(${fn} ${args.map(elispArgument).join(" ")})`;
  const temporary = await mkdtemp(join(tmpdir(), "opencode-emacs-"));
  const output = join(temporary, "result");
  try {
    // Bootstrap BEFORE calling cet-eval-to-file: the original OMP bridge calls
    // that function before loading the file that defines it on a cold server.
    const form = `(progn (require 'subr-x) (require 'org) ` +
      `(unless (featurep 'emacs-tools) (load ${elispString(options.elispPath)} nil t)) ` +
      (fn === "opencode-emacs-cursor-context"
        ? `(load ${elispString(options.cursorContextPath!)} nil t) `
        : "") +
      `(cet-eval-to-file ${elispString(output)} ` +
      `'(let ((default-directory ${elispString(resolve(cwd) + sep)})) ${call})))`;
    try {
      await run(options.emacsclient ?? "emacsclient", ["--alternate-editor=false", "--eval", form], {
        timeout: options.timeout ?? 10_000,
        encoding: "utf-8",
        signal,
      });
      signal?.throwIfAborted();
      return await readFile(output, "utf-8");
    } catch (error) {
      signal?.throwIfAborted();
      const message = error instanceof Error ? error.message : String(error);
      throw new Error(`emacsclient error: ${message}`, { cause: error });
    }
  } finally {
    await rm(temporary, { recursive: true, force: true });
  }
}
