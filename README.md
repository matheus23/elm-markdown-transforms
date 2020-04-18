# Elm-markdown-transforms

See the documentation at [package.elm-lang.org](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/).

This package is for working with [elm-markdown](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/). It aims to make it easier for the users of elm-markdown to write more sophisticated [Renderer](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/Markdown-Renderer)s.

Some examples of tasks with markdown that this library may make easier for you:

* Rendering markdown to functions: If you want your markdown to be interactive (have `Model` access), you need to render to a function. Take a look at [`parameterized`](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/Markdown-Scaffolded#parameterized).
* Validating markdown in the renderer: See [`validated`](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/Markdown-Scaffolded#validated).
* Making elm-pages [`StaticHttp.Request`](https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/Pages-StaticHttp#Request)s in your renderer: See [`withStaticHttpRequests`](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/Markdown-Scaffolded#withStaticHttpRequests).
* Accumulating information about your markdown files: See [`reduce`](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/Markdown-Scaffolded#reduce).

I highly recommend taking a look at the module documentation for [`Markdown.Scaffolded`](https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/Markdown-Scaffolded).

## Examples

**Browse through some examples**:

* Generating Slugs
* Checking Links for validity
  * Via Http
  * Via StaticHttp
* Rendering to function `Model -> Html Msg`
* Generating a word-count
* Extracting code blocks
* Custom Html thingies
  * Quizzes
  * Carusel?

## Discussion/Contact

Feel free to open an issue, if you want to discuss something.

Other than that, you can DM me @matheus23 in the [elm slack](https://elm-lang.org/community).

## Indirect Dependency Philosophy

You might notice, that this package has quite some dependencies, which would be introduced to all applications that depend on this package in their so-called 'indirect dependencies' (this is a field in the `elm.json` file).

Essentially this has following downside: If any of your application's indirect dependencies aren't compatible, you can't use both together.

Therefore, keeping a package's dependency footprint as small as possible might seem like the right way.

However, in this package I'm experimenting with a different philosophy for following reasons:

* Any code that your application doesn't use gets optimized away by Elm's compiler when run with the `--optimize` flag. So if you're worrying that the elm-pages indirect dependency introduced by your application that doesn't even _use_ elm-pages will have any impact in production, it won't!
* The alternative to having a dependency is duplicating code that exists somewhere else. This can result in duplicated code in your compiled .js file.
* More philosophical: I believe packages and applications should all run on as up-to-date dependencies as possible. Elm's static typing makes updating to new package versions a breeze, even if their API changed. Even if the maintainer of a package hasn't updated the package's dependencies, it is in many cases possible for anyone with Elm experience to upgrade the dependency and submit a pull request.

If you are looking for a tool that helps you update to the latest package versions, I recommend (the confusingly named) [zwilias/elm-json](https://github.com/zwilias/elm-json#readme).

I am aware that this decision might be controversial for good reasons. After all, we track dependencies precisely for being able to depend on older versions than the most recent one! And there might be good reasons for depending on older versions, if you just can't upgrade to a newer version for some reason that is out of your control.

If there are any downsides I missed, of if you just don't agree with my prioritization or anything about this matter, please reach out to me.
