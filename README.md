# simghost

Simple image hosting script, written in Haskell.

Features:

* Can upload images either from files or from the clipboard.
* Uses a username/password system to avoid spammers and unauthorized uploads.
* Works as a CGI script in a single-file binary.

## Compilation (static)

The default compilation mode is to produce a static binary. This requires a
static version of `libgmp` to be installed (e.g. with `libgmp-static` on Arch).

If you cannot install a static version of `libgmp` system-wide, you can compile
it yourself and add the following lines to `package.yaml`:

```yaml
extra-lib-dirs:
- /path/to/gmp-6.2.0/.libs
```

So that `libgmp.a` can be found. You can also pass this as a command line flag
to `stack`: `stack build --extra-lib-dirs=/path/to/...`.

Afterwards, just run `stack build` to build the executable in
`.stack-work/install/...`.

## Compilation (dynamic)

If you do not want to use static compilation and would rather link the
libraries dynamically, you can remove the `-static` from `ghc-options`,
`cc-options` and `ld-options` in `package.yaml`:

```diff
--- a/package.yaml
+++ b/package.yaml
@@ -60,6 +60,5 @@ tests:
     dependencies:
     - simghost
 
-ghc-options: -Wall -O2 -static -threaded
-cc-options: -static
-ld-options: -static -pthread
+ghc-options: -Wall -O2 -threaded
+ld-options: -pthread
```

After that, just run `stack build` to build the executable.

## Installing & Configuration

* Copy the executable to the path where you want the script to be accessible.
* Make sure that your webserver executes the script as CGI.
* Set up the configuration in `settings.json` (same directory as the script):

```json
{
    "outputDir": "images/",
    "users": {
        "foo": "$2y$12$XiBSIoGnHcfaztfxEXe/uuCwuC5PEPlEIhavL28t4Kph/LLJYskke"
    }
}
```

(The passwords are bcrypt hashed)

* Make sure that the output directory exists.
* Congratulations, it should work!

For extra security, you should configure your webserver to deny access to at
least the `settings.json` document. Preferably, you should also disable the
directory listing for your output directory and deny access to any `*.meta`
files.

As an example for Apache2, the following `.htaccess` can be used:

```
AddHandler cgi-script .cgi
DirectoryIndex simghost.cgi
Options -Indexes

<Files settings.json>
Deny from all
</Files>

<FilesMatch "\.meta$">
Deny from all
</FilesMatch>
```
