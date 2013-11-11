module Foundation where

import           Yesod
import           Yesod.Default.Util
import           Yesod.Form.Jquery

{-

Every Yesod application has a foundation data type. This is a data type
which represents the application itself. It can include any initialization
data necessary for running the application, such as a database connection pool.

In our case, our application has no such initialization data, and therefore
our datatype is very simplistic. By convention, we call this datatype "App" with
a single data constructor, also called "App".

-}
data App = App

{-

The Yesod typeclass allows us to alter a number of the behaviors of Yesod, such
as the default page layout, error handler functions, and how URLs are rendered.
We're going to accept all of the defaults in our application, so our instance
is pretty boring.

-}
instance Yesod App

{-

Every Yesod application has a set of routes associated with its foundation datatype.
These routes represent all of the URLs that can be accessed in an application.
Instead of manually defining the route datatype and its parse/render/dispatch functions,
Yesod uses code generation to avoid boilerplate and ensure that the parse/render combination
is both correct and efficient.

parseRoutes parses Yesod's routing Domain Specific Language (DSL), and mkYesodData generates
the route datatype and the parse/render functions. We'll see the generation of the dispatch
function when we get to the Main module.

Our application has three routes. HomeR is served from /, and responds to GET requests.
MarkdownR is served from /markdown, and answers PUT requests. FibR has a dynamic component,
and responds to any GET request made to /fib/<some integer>. Yesod automatically handles
the parsing of these routes into Haskell datatypes, so your application doesn't need to
worry about input validation or marshaling.

Each route has a corresponding handler module, e.g. Handler.Home.
Please see those modules for more details.

-}
mkYesodData "App" [parseRoutes|
/         HomeR     GET
/markdown MarkdownR PUT
/fib/#Int FibR      GET
|]
