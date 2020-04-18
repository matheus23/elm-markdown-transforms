module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    todo """Unfortunately, there are not tests, yet. You're welcome to contribute tests!
    I personally test this project using the /examples and another semi-production app.
    
    If you want to contribute: There are good opportunities for creating property-based
    tests for this application. For that it would be neccessary to write a fuzzer:
    `Fuzzer children -> Fuzzer (Scaffolded.Block children)` and then identify the
    mathematical properties of functions like `map`, `foldFunction`, etc. and test them.
    """
