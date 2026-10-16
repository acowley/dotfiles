import type { Plugin } from "@opencode/plugin";
import { fileURLToPath } from "node:url";
import { emacsEval, type ElispArgument } from "./bridge";

const ELISP_PATH = fileURLToPath(new URL("../../elisp/emacs-tools.el", import.meta.url));
const CURSOR_CONTEXT_PATH = fileURLToPath(new URL("./cursor-context.el", import.meta.url));

const filepath = {
  type: "string",
  minLength: 1,
  description: "File path, absolute or relative to the session's working directory",
} as const;

const line = (description: string) => ({ type: "integer", minimum: 1, description } as const);

interface Parameters {
  filepath: string;
  filepaths: string[];
  start_line: number;
  end_line: number;
  max_depth?: number;
  heading_pattern: string;
  line_number: number;
  pattern: string;
}

interface Definition {
  name: string;
  description: string;
  properties: Record<string, object>;
  required: string[];
  fn: string;
  args: (params: Parameters) => ElispArgument[];
}

const definitions: Definition[] = [
  {
    name: "open_image",
    description: "Open an image file in Emacs without stealing focus. Use this to show generated figures, plots, or screenshots.",
    properties: { filepath },
    required: ["filepath"],
    fn: "omp-open-image",
    args: (p) => [p.filepath],
  },
  {
    name: "open_images",
    description: "Open multiple images in Emacs's existing OMP image viewer for comparison. Replaces the viewer's image list.",
    properties: { filepaths: { type: "array", items: filepath, minItems: 1, description: "Image file paths" } },
    required: ["filepaths"],
    fn: "omp-open-images",
    args: (p) => [p.filepaths],
  },
  {
    name: "flash_code",
    description: "Open a file in Emacs and briefly highlight an inclusive range of lines, without stealing focus. Use this to draw attention to specific code after making changes.",
    properties: { filepath, start_line: line("Starting line (1-based)"), end_line: line("Ending line (1-based, inclusive)") },
    required: ["filepath", "start_line", "end_line"],
    fn: "omp-pulse-region",
    args: (p) => {
      if (p.end_line < p.start_line) throw new Error("end_line must be at least start_line");
      return [p.filepath, p.start_line, p.end_line];
    },
  },
  {
    name: "org_list_headings",
    description: "List Org headings with line numbers as an indented outline. Use this first to understand an Org file. Reads unsaved contents when the file is already open in Emacs.",
    properties: { filepath, max_depth: line("Maximum heading depth; omit to include all depths") },
    required: ["filepath"],
    fn: "omp-org-list-headings",
    args: (p) => [p.filepath, p.max_depth],
  },
  {
    name: "org_get_subtree",
    description: "Get the entire Org subtree (heading, content, and descendants) under the first heading matching an Emacs regular expression. Reads unsaved contents from existing buffers.",
    properties: { filepath, heading_pattern: { type: "string", description: "Emacs regular expression matching the heading title" } },
    required: ["filepath", "heading_pattern"],
    fn: "omp-org-get-subtree",
    args: (p) => [p.filepath, p.heading_pattern],
  },
  {
    name: "org_get_heading_at_line",
    description: "Get the Org subtree starting at a specific line. Use after org_list_headings to fetch a section by its line number. Reads unsaved contents from existing buffers.",
    properties: { filepath, line_number: line("Line where the heading starts (1-based)") },
    required: ["filepath", "line_number"],
    fn: "omp-org-get-heading-at-line",
    args: (p) => [p.filepath, p.line_number],
  },
  {
    name: "org_search_headings",
    description: "Search Org headings using an Emacs regular expression. Returns matching headings with line numbers. Reads unsaved contents from existing buffers.",
    properties: { filepath, pattern: { type: "string", description: "Emacs regular expression to search for in heading titles" } },
    required: ["filepath", "pattern"],
    fn: "omp-org-search-headings",
    args: (p) => [p.filepath, p.pattern],
  },
  {
    name: "cursor_context",
    description: "Report the selected Emacs window's buffer, file, line, column, and Org heading without changing editor state.",
    properties: {},
    required: [],
    fn: "opencode-emacs-cursor-context",
    args: () => [],
  },
];

// Type-only SDK import: OpenCode can load this local plugin without installing
// a second copy of its runtime or any npm dependencies.
export default {
  id: "acowley.emacs-tools",
  async setup(ctx) {
    const options = {
      elispPath: typeof ctx.options.elispPath === "string" ? ctx.options.elispPath : ELISP_PATH,
      emacsclient: typeof ctx.options.emacsclient === "string" ? ctx.options.emacsclient : undefined,
      cursorContextPath: CURSOR_CONTEXT_PATH,
    };
    await ctx.tool.transform((editor) => {
      editor.namespace({ name: "emacs", description: "Emacs image viewing, code highlighting, Org-mode queries, and cursor context via emacsclient" });
      for (const definition of definitions) {
        editor.add({
          name: definition.name,
          description: definition.description,
          input: { type: "object", properties: definition.properties, required: definition.required, additionalProperties: false },
          options: { namespace: "emacs", codemode: true, permission: `emacs_${definition.name}` },
          async execute(input, context) {
            const args = definition.args(input as Parameters);
            // A plugin can serve more than one session; don't capture setup's cwd.
            const session = await ctx.session.get({ sessionID: context.sessionID });
            const content = await emacsEval(options, session.location.directory, definition.fn, args, context.signal);
            return { content };
          },
        });
      }
    });
  },
} satisfies Plugin.Plugin;
