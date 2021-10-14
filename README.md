Emacs package to browse Chrome bookmarks with [Ivy](https://github.com/abo-abo/swiper).

> :warning: Soft requirement: [`jq`](https://stedolan.github.io/jq/). 
> Counsel-chrome-bm will work without jq, but it will be slow when you have a lot of bookmarks.

`counsel-chrome-bm` provides the commands **`counsel-chrome-bm`** and **`counsel-chrome-bm-all`**
to browse your Chrom(e/ium) bookmarks and provides some default actions to perform on
a selected bookmark.

By default `counsel-chrome-bm` uses **`jq`** if jq is available and falls back to
Emacs' builtin JSON-parsing otherwise. Using jq is fast and asynchronous, but it
doesn't provide fuzzy matching. Using Emacs' JSON-parsing can be slow when you have
a lot of bookmarks or a large bookmark file (some browsers store icons in the
bookmark file), but it uses Ivy's own (fuzzy) matching.

Most Chromium-based browsers should work too, including Edge, Vivaldi and Brave. 
Opera is currently not supported.

`counsel-chrome-bm` respects the ignore lists `counsel-chrome-bm-ignore-folder`
and `counsel-chrome-bm-ignore-key`. `counsel-chrome-bm-all` will show *all* 
bookmarks.

After selecting a bookmark, a list of actions to choose from is shown, unless
`counsel-chrome-bm-default-action` is set. Calling the commands with a prefix
argument, the list of actions will always be shown, regardless of any default
action.

# Configuration

**`counsel-chrome-bm-file`** (default: depends)<br />
The bookmark file to get bookmarks from.

**`counsel-chrome-bm-jq`** (default: `nil`)<br />
Path of the `jq` executable, if jq is not on the PATH.

**`counsel-chrome-bm-ignore-folder`** (default: `nil`)<br />
A list of folder names (strings). Bookmarks in these folders (and any subfolders) will be 
ignored by `counsel-chrome-bm`. Case-sensitive.

**`counsel-chrome-bm-ignore-key`** (default: `'("trash")`)<br />
A list of keys of the 'roots' object in the bookmarks file which should be
ignored. Case-sensitive. Bookmarks under these keys and any of their folders 
will not be included. Standard keys in a Chrome bookmark file are 'bookmark_bar', 
'synced' and 'other'. Some Chromium based browsers add additional keys.

**`counsel-chrome-bm-default-action`** (default: `nil`)<br />
Default action to execute after selecting a bookmark. Must be a function which 
will receive two string arguments.  The first is the title, the second the url 
of the selected bookmark.

See `counsel-chrome-bm-actions-alist` for a number of provided actions.

When nil, a selection of actions to choose from will be shown.
When `counsel-chrome-bm` or `counsel-chrome-bm-all` are executed with a prefix
argument the selection of actions will always be shown.

**`counsel-chrome-bm-actions-alist`** (default: depends)<br />
Alist of possible actions after selecting a bookmark.

When called with a prefix argument or when `counsel-chrome-bm-default-action` is
nil, `counsel-chrome-bm` and `counsel-chrome-bm-all` will prompt which of these
action to perform on the selected bookmark.

The car is the name of the action to display and the cdr is a function which
will receive 2 string arguments, the first being the title of the bookmark,
the second the url.

**`counsel-chrome-bm-url-prefix`** (default: `" - "`)<br />
A string which is displayed before the url part of a bookmark.

**`counsel-chrome-bm-url-suffix`** (default: `""`)<br />
A string which is displayed after the url part of a bookmark.
