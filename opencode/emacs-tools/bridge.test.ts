import { afterAll, beforeAll, expect, setDefaultTimeout, test } from "bun:test";
import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { elispString, emacsEval } from "./bridge";
import plugin from "./index";

// Starting pristine batch Emacs is slower than talking to a running server.
setDefaultTimeout(30_000);

let directory: string;
let client: string;
const elispPath = fileURLToPath(new URL("../../elisp/emacs-tools.el", import.meta.url));
const cursorContextPath = fileURLToPath(new URL("./cursor-context.el", import.meta.url));

beforeAll(async () => {
  directory = await mkdtemp(join(tmpdir(), "opencode-emacs-test-"));
  client = join(directory, "batch-emacsclient");
  // emacsEval passes --alternate-editor=false, --eval, FORM. Each invocation starts
  // a pristine Emacs to exercise bootstrap without touching the user's server.
  await writeFile(client, '#!/bin/sh\n[ "$1" = "--alternate-editor=false" ] || exit 1\nexec emacs --batch -Q --eval "$3"\n', { mode: 0o700 });
  await writeFile(join(directory, "notes.org"), "* Hardware\nMain content.\n** Camera\nCamera details.\n* Software\nFirmware details.\n");
  await writeFile(join(directory, "cursor.txt"), "one\ntwo\nthree\nfour\n");
});

afterAll(async () => {
  await rm(directory, { recursive: true, force: true });
});

function evaluate(fn: string, args: Parameters<typeof emacsEval>[3]) {
  return emacsEval({ elispPath, emacsclient: client }, directory, fn, args);
}

async function evaluateCursor(preload: string) {
  const preloadPath = join(directory, `cursor-preload-${Math.random().toString(16).slice(2)}.el`);
  const cursorClient = `${preloadPath}.client`;
  await writeFile(preloadPath, preload);
  await writeFile(cursorClient, `#!/bin/sh\nexec emacs --batch -Q --load '${preloadPath}' --eval "$3"\n`, { mode: 0o700 });
  try {
    return await emacsEval({ elispPath, cursorContextPath, emacsclient: cursorClient }, directory, "opencode-emacs-cursor-context", []);
  } finally {
    await rm(preloadPath, { force: true });
    await rm(cursorClient, { force: true });
  }
}

test("escape Lisp strings without changing regex backslashes", () => {
  expect(elispString('quote" slash\\ newline\nreturn\rtab\t')).toBe('"quote\\" slash\\\\ newline\\nreturn\\rtab\\t"');
});

test("cold start loads the bridge and Org; relative paths have directory semantics", async () => {
  expect(await evaluate("omp-org-list-headings", ["notes.org"])).toBe("1:Hardware\n3:  Camera\n5:Software");
  expect(await evaluate("omp-org-list-headings", ["notes.org", 1])).toBe("1:Hardware\n5:Software");
});

test("subtree and line queries preserve the trailing newline", async () => {
  expect(await evaluate("omp-org-get-subtree", ["notes.org", "^Hardware$"])).toBe("* Hardware\nMain content.\n** Camera\nCamera details.\n");
  expect(await evaluate("omp-org-get-heading-at-line", ["notes.org", 3])).toBe("** Camera\nCamera details.\n");
  expect(await evaluate("omp-org-get-heading-at-line", ["notes.org", 2])).toBe("No heading at line 2");
});

test("search uses Emacs regex syntax and reports absent headings", async () => {
  expect(await evaluate("omp-org-search-headings", ["notes.org", "\\(Hardware\\|Software\\)"])).toBe("1: Hardware\n5: Software");
  expect(await evaluate("omp-org-search-headings", ["notes.org", "Missing"])).toBe("No headings matching 'Missing'");
});

test("filenames containing quotes and backslashes are not interpreted as Lisp", async () => {
  const filename = 'quoted" back\\slash.org';
  await writeFile(join(directory, filename), "* Quoted\nText.\n");
  expect(await evaluate("omp-org-list-headings", [filename])).toBe("1:Quoted");
});

test("Org queries prefer unsaved buffer content", async () => {
  const preload = join(directory, "preload.el");
  await writeFile(preload, `(progn (find-file ${elispString(join(directory, "notes.org"))}) (goto-char (point-max)) (insert "* Unsaved\\n"))`);
  const unsavedClient = join(directory, "unsaved-emacsclient");
  await writeFile(unsavedClient, `#!/bin/sh\nexec emacs --batch -Q --load '${preload}' --eval "$3"\n`, { mode: 0o700 });
  const result = await emacsEval({ elispPath, emacsclient: unsavedClient }, directory, "omp-org-list-headings", ["notes.org"]);
  expect(result).toContain("7:Unsaved");
});

test("image list arguments are quoted and passed intact", async () => {
  const filename = 'image" one.svg';
  await writeFile(join(directory, filename), "<svg xmlns=\"http://www.w3.org/2000/svg\"/>");
  const preload = join(directory, "images.el");
  await writeFile(preload, `(load ${elispString(elispPath)} nil t) (defun omp-image--display-current () (mapconcat #'identity omp-image-list "\\n"))`);
  const imageClient = join(directory, "image-emacsclient");
  await writeFile(imageClient, `#!/bin/sh\nexec emacs --batch -Q --load '${preload}' --eval "$3"\n`, { mode: 0o700 });
  const result = await emacsEval({ elispPath, emacsclient: imageClient }, directory, "omp-open-images", [[filename, filename]]);
  expect(result).toBe(`${join(directory, filename)}\n${join(directory, filename)}`);
  expect(await emacsEval({ elispPath, emacsclient: imageClient }, directory, "omp-open-image", [filename])).toBe(join(directory, filename));
});

test("highlighting calls the existing pulse tool", async () => {
  expect(await evaluate("omp-pulse-region", ["notes.org", 1, 3])).toBe("Highlighted notes.org lines 1-3");
});

test("cursor context reports the selected file buffer and one-based line and column", async () => {
  const result = await evaluateCursor(`(find-file ${elispString(join(directory, "cursor.txt"))})
(goto-char (point-min)) (forward-line 1) (forward-char 2)`);
  expect(result).toContain(`Buffer: cursor.txt\nFile: ${join(directory, "cursor.txt")}\nLine: 2\nColumn: 3`);
});

test("cursor context falls back from OpenCode Agent to the most recently visited buffer", async () => {
  const result = await evaluateCursor(`(define-derived-mode agent-shell-mode fundamental-mode "Agent Shell")
(find-file ${elispString(join(directory, "cursor.txt"))})
(goto-char (point-min)) (forward-line 2) (forward-char 1)
(switch-to-buffer (get-buffer-create "OpenCode Agent")) (agent-shell-mode)`);
  expect(result).toContain(`Buffer: cursor.txt\nFile: ${join(directory, "cursor.txt")}\nLine: 3\nColumn: 2`);
});

test("cursor context reports non-file buffers", async () => {
  const result = await evaluateCursor(`(switch-to-buffer (get-buffer-create "*cursor-scratch*"))
(erase-buffer) (insert "first\\nsecond") (goto-char (point-max))`);
  expect(result).toContain("Buffer: *cursor-scratch*\nFile: (none)\nLine: 2\nColumn: 7");
});

test("cursor context counts absolute lines in a narrowed buffer", async () => {
  const result = await evaluateCursor(`(find-file ${elispString(join(directory, "cursor.txt"))})
(goto-char (point-min)) (forward-line 2) (let ((start (point))) (forward-line 2) (narrow-to-region start (point)))
(goto-char (point-max)) (backward-char 1)`);
  expect(result).toContain("Line: 4\nColumn: 5");
});

test("cursor context reports the containing Org heading and preserves point", async () => {
  const statePath = join(directory, "cursor-state");
  const result = await evaluateCursor(`(load ${elispString(elispPath)} nil t)
(find-file ${elispString(join(directory, "notes.org"))})
(goto-char (point-min)) (forward-line 3) (forward-char 4)
(defun test-cursor-context-around (original &rest args)
  (let* ((window (selected-window)) (buffer (window-buffer window))
         (point-before (with-current-buffer buffer (point)))
         (window-point-before (window-point window))
         (result (apply original args))
         (point-after (with-current-buffer buffer (point)))
         (window-point-after (window-point window)))
    (with-temp-file ${elispString(statePath)}
      (insert (format "%s %s" point-before window-point-before) "|"
              (format "%s %s" point-after window-point-after)))
    result))
(advice-add 'opencode-emacs-cursor-context :around #'test-cursor-context-around)`);
  expect(result).toContain("Org heading: Camera");
  const state = await Bun.file(statePath).text();
  const [before, after] = state.split("|");
  expect(after).toBe(before);
});

test("cursor helper bootstraps when the shared OMP feature is already loaded", async () => {
  const result = await evaluateCursor(`(load ${elispString(elispPath)} nil t)
(switch-to-buffer (get-buffer-create "*preloaded-omp*")) (insert "ready") (goto-char (point-max))`);
  expect(result).toContain("Buffer: *preloaded-omp*");
  expect(result).toContain("Line: 1\nColumn: 6");
});

test("missing files and Lisp errors are returned without a missing-result failure", async () => {
  expect(await evaluate("omp-org-list-headings", ["missing.org"])).toContain("Error: File not found:");
  expect(await evaluate("omp-nonexistent", [])).toContain("Error:");
});

test("client failures report an error and remove private result directories", async () => {
  const before = (await readdir(tmpdir())).filter((name) => name.startsWith("opencode-emacs-"));
  await expect(emacsEval({ elispPath, emacsclient: join(directory, "absent-client") }, directory, "omp-open-image", ["x"])).rejects.toThrow("emacsclient error:");
  const after = (await readdir(tmpdir())).filter((name) => name.startsWith("opencode-emacs-"));
  expect(after.sort()).toEqual(before.sort());
});

test("timeouts and cancellation stop client execution", async () => {
  const slowClient = join(directory, "slow-emacsclient");
  await writeFile(slowClient, "#!/bin/sh\nexec sleep 60\n", { mode: 0o700 });
  await expect(emacsEval({ elispPath, emacsclient: slowClient, timeout: 50 }, directory, "omp-open-image", ["x"])).rejects.toThrow("emacsclient error:");
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), 50);
  try {
    await expect(emacsEval({ elispPath, emacsclient: slowClient }, directory, "omp-open-image", ["x"], controller.signal)).rejects.toThrow();
  } finally {
    clearTimeout(timer);
  }
});

test("registers all eight names and uses the invoking session's directory", async () => {
  const tools: any[] = [];
  await plugin.setup({
    options: { emacsclient: client },
    location: { directory: "/not-the-session-directory" },
    session: { get: async () => ({ location: { directory } }) },
    tool: { transform: async (callback: any) => callback({ namespace: () => {}, add: (tool: any) => tools.push(tool) }) },
  } as any);
  expect(tools.map((tool) => `emacs_${tool.name}`)).toEqual([
    "emacs_open_image", "emacs_open_images", "emacs_flash_code", "emacs_org_list_headings",
    "emacs_org_get_subtree", "emacs_org_get_heading_at_line", "emacs_org_search_headings",
    "emacs_cursor_context",
  ]);
  for (const tool of tools) {
    expect(tool.options.namespace).toBe("emacs");
    expect(tool.options.codemode).toBe(true);
    expect(tool.options.permission).toBe(`emacs_${tool.name}`);
    expect(tool.input.additionalProperties).toBe(false);
  }
  const context = { sessionID: "ses_test", signal: new AbortController().signal };
  const outline = tools.find((tool) => tool.name === "org_list_headings");
  expect(await outline.execute({ filepath: "notes.org" }, context)).toEqual({ content: "1:Hardware\n3:  Camera\n5:Software" });
  const flash = tools.find((tool) => tool.name === "flash_code");
  await expect(flash.execute({ filepath: "notes.org", start_line: 3, end_line: 1 }, context)).rejects.toThrow("end_line must be at least start_line");
  const cursor = tools.find((tool) => tool.name === "cursor_context");
  expect(cursor.input.properties).toEqual({});
  expect(cursor.input.required).toEqual([]);
  expect(await cursor.execute({}, context)).toEqual({ content: expect.stringContaining("Buffer:") });
});
