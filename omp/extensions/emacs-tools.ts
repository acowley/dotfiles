import type { ExtensionAPI } from "@oh-my-pi/pi-coding-agent";
import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const ELISP_PATH = join(
  import.meta.dirname,
  "..",
  "emacs-tools.el",
);

function elispEscape(s: string): string {
  return s.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}

function ensureLoaded(): string {
  return `(unless (featurep 'omp-emacs-tools) (load "${elispEscape(ELISP_PATH)}"))`;
}

/** Evaluate an Elisp form via emacsclient, return the result string. */
function emacsEvalForm(cwd: string, form: string): string {
  const tmpDir = mkdtempSync(join(tmpdir(), "omp-emacs-"));
  const outFile = join(tmpDir, "result");
  try {
    const fullForm =
      `(cet-eval-to-file "${outFile}" ` +
      `'(progn ${ensureLoaded()} ` +
      `(let ((default-directory "${elispEscape(cwd)}")) ${form})))`;
    try {
      execFileSync("emacsclient", ["--eval", fullForm], {
        timeout: 10_000,
        encoding: "utf-8",
      });
    } catch (err: unknown) {
      const msg = err instanceof Error ? err.message : String(err);
      return `emacsclient error: ${msg}`;
    }
    return readFileSync(outFile, "utf-8").trim();
  } finally {
    rmSync(tmpDir, { recursive: true, force: true });
  }
}

/** Build `(fn arg1 arg2 ...)` and evaluate. */
function emacsEval(cwd: string, fn: string, args: unknown[]): string {
  const elispArgs = args
    .map((a) => {
      if (typeof a === "string") return `"${elispEscape(a)}"`;
      if (typeof a === "number") return String(Math.round(a));
      if (a === null || a === undefined) return "nil";
      return String(a);
    })
    .join(" ");
  return emacsEvalForm(cwd, `(${fn} ${elispArgs})`);
}

export default function emacsTools(pi: ExtensionAPI) {
  const { z } = pi.zod;

  pi.setLabel("Emacs Tools");

  pi.registerTool({
    name: "emacs_open_image",
    label: "Open Image in Emacs",
    description:
      "Open an image file in Emacs for viewing. " +
      "Use this to show generated figures, plots, or screenshots.",
    parameters: z.object({
      filepath: z.string().describe(
        "Path to the image file, absolute or relative to the project root",
      ),
    }),
    async execute(_id, params, _signal, _onUpdate, ctx) {
      const result = emacsEval(ctx.cwd, "omp-open-image", [params.filepath]);
      return {
        content: [{ type: "text", text: result }],
        details: { tool: "open_image", filepath: params.filepath },
      };
    },
  });

  pi.registerTool({
    name: "emacs_open_images",
    label: "Open Images in Emacs",
    description:
      "Open multiple image files in Emacs for cycling through them. " +
      "Use this to compare several generated figures side by side.",
    parameters: z.object({
      filepaths: z.array(z.string()).describe("Array of image file paths"),
    }),
    async execute(_id, params, _signal, _onUpdate, ctx) {
      const quoted = params.filepaths
        .map((p: string) => `"${elispEscape(p)}"`)
        .join(" ");
      const result = emacsEvalForm(
        ctx.cwd,
        `(omp-open-images '(${quoted}))`,
      );
      return {
        content: [{ type: "text", text: result }],
        details: { tool: "open_images", count: params.filepaths.length },
      };
    },
  });

  pi.registerTool({
    name: "emacs_flash_code",
    label: "Flash Region in Emacs",
    description:
      "Open a file in Emacs and briefly highlight a region of lines. " +
      "Use this to draw attention to specific code after making changes.",
    parameters: z.object({
      filepath: z.string().describe("Path to the file"),
      start_line: z.number().int().min(1).describe(
        "Starting line number (1-based)",
      ),
      end_line: z.number().int().min(1).describe(
        "Ending line number (1-based, inclusive)",
      ),
    }),
    async execute(_id, params, _signal, _onUpdate, ctx) {
      const result = emacsEval(ctx.cwd, "omp-pulse-region", [
        params.filepath,
        params.start_line,
        params.end_line,
      ]);
      return {
        content: [{ type: "text", text: result }],
        details: {
          tool: "flash_code",
          filepath: params.filepath,
          lines: `${params.start_line}-${params.end_line}`,
        },
      };
    },
  });

  pi.registerTool({
    name: "emacs_org_list_headings",
    label: "List Org Headings",
    description:
      "List all headings in an org file with line numbers. " +
      "Returns an indented outline showing document structure. " +
      "Use this first to understand the structure of an org file.",
    parameters: z.object({
      filepath: z.string().describe("Path to the org file"),
      max_depth: z.number().int().min(1).optional().describe(
        "Maximum heading depth to include (optional, defaults to all)",
      ),
    }),
    async execute(_id, params, _signal, _onUpdate, ctx) {
      const args: unknown[] = [params.filepath];
      if (params.max_depth !== undefined) args.push(params.max_depth);
      const result = emacsEval(ctx.cwd, "omp-org-list-headings", args);
      return {
        content: [{ type: "text", text: result }],
        details: { tool: "org_list_headings", filepath: params.filepath },
      };
    },
  });

  pi.registerTool({
    name: "emacs_org_get_subtree",
    label: "Get Org Subtree",
    description:
      "Get the entire subtree (heading + all content + all subheadings) " +
      "under a heading matching a regex pattern. " +
      "Use when you need a section and all its children.",
    parameters: z.object({
      filepath: z.string().describe("Path to the org file"),
      heading_pattern: z.string().describe(
        "Regex pattern to match the heading title",
      ),
    }),
    async execute(_id, params, _signal, _onUpdate, ctx) {
      const result = emacsEval(ctx.cwd, "omp-org-get-subtree", [
        params.filepath,
        params.heading_pattern,
      ]);
      return {
        content: [{ type: "text", text: result }],
        details: {
          tool: "org_get_subtree",
          filepath: params.filepath,
          pattern: params.heading_pattern,
        },
      };
    },
  });

  pi.registerTool({
    name: "emacs_org_get_heading_at_line",
    label: "Get Org Heading at Line",
    description:
      "Get the subtree starting at a specific line number. " +
      "Use after org_list_headings to fetch a section by its line number.",
    parameters: z.object({
      filepath: z.string().describe("Path to the org file"),
      line_number: z.number().int().min(1).describe(
        "Line number where the heading starts",
      ),
    }),
    async execute(_id, params, _signal, _onUpdate, ctx) {
      const result = emacsEval(ctx.cwd, "omp-org-get-heading-at-line", [
        params.filepath,
        params.line_number,
      ]);
      return {
        content: [{ type: "text", text: result }],
        details: {
          tool: "org_get_heading_at_line",
          filepath: params.filepath,
          line: params.line_number,
        },
      };
    },
  });

  pi.registerTool({
    name: "emacs_org_search_headings",
    label: "Search Org Headings",
    description:
      "Search for headings matching a regex pattern in an org file. " +
      "Returns matching headings with line numbers.",
    parameters: z.object({
      filepath: z.string().describe("Path to the org file"),
      pattern: z.string().describe(
        "Regex pattern to search for in heading titles",
      ),
    }),
    async execute(_id, params, _signal, _onUpdate, ctx) {
      const result = emacsEval(ctx.cwd, "omp-org-search-headings", [
        params.filepath,
        params.pattern,
      ]);
      return {
        content: [{ type: "text", text: result }],
        details: {
          tool: "org_search_headings",
          filepath: params.filepath,
          pattern: params.pattern,
        },
      };
    },
  });
}
