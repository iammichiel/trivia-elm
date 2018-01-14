module Main exposing (..)

import Html exposing (Html, a, button, div, h1, h2, li, p, span, text, ul)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Navigation exposing (Location, program)
import Random
import UrlParser exposing (parseHash, s, Parser, oneOf, map, top, (</>))
import Http exposing (..)
import Task exposing (Task, andThen)
import Json.Decode exposing (field, int, list, map2, map3, map4, string)
import Random.List
import Random.Extra
import ElmEscapeHtml exposing (unescape)


type Route
    = HomeRoute
    | GameRoute (Maybe Int)
    | CategoriesRoute
    | ResultRoute Int


type alias Model =
    { game : RemoteData Game
    , route : Route
    , categories : RemoteData (List Category)
    , categoryIdMaybe : Maybe Int
    }


type alias Game =
    { currentQuestion : Question
    , questions : List Question
    , answeredQuestions : List AnsweredQuestion
    }


type alias Question =
    { question : String
    , correctAnswer : String
    , answers : List String
    }


type alias AnsweredQuestion =
    { question : Question
    , result : QuestionResult
    }


type QuestionResult
    = Correct
    | Incorrect


type alias Category =
    { id : Int, name : String }


type RemoteData a
    = NotLoaded
    | Loaded a
    | OnError Error


type Msg
    = OnLocationChange Location
    | OnCategoriesFetched (Result Error (List Category))
    | OnQuestionsFetched (Result Error (List Question))
    | OnQuestionsShuffled (List Question)
    | AnswerQuestion String


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map HomeRoute top
        , map (GameRoute Nothing) (s "game")
        , map (Just >> GameRoute) (s "game" </> s "category" </> UrlParser.int)
        , map CategoriesRoute (s "categories")
        , map ResultRoute (s "result" </> UrlParser.int)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            HomeRoute


initialModel : Route -> Model
initialModel route =
    { game = NotLoaded
    , categories = NotLoaded
    , categoryIdMaybe = Nothing
    , route = route
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        route =
            parseLocation location

        cmd =
            case route of
                GameRoute _ ->
                    Cmd.batch [ Http.send OnCategoriesFetched getCategories, getQuestionsRequest ]

                _ ->
                    Http.send OnCategoriesFetched getCategories
    in
        ( parseLocation location |> initialModel, cmd )


main : Program Never Model Msg
main =
    program OnLocationChange { init = init, update = update, view = view, subscriptions = (\m -> Sub.none) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            case parseLocation location of
                GameRoute categoryIdMaybe ->
                    ( { model
                        | categoryIdMaybe = categoryIdMaybe
                        , game = NotLoaded
                        , route = GameRoute categoryIdMaybe
                      }
                    , Http.send OnQuestionsFetched (getQuestions categoryIdMaybe)
                    )

                route ->
                    ( { model | route = route }, Cmd.none )

        OnQuestionsFetched (Ok questions) ->
            ( model, Random.generate OnQuestionsShuffled (shuffleQuestions questions) )

        OnQuestionsFetched (Err error) ->
            ( { model | game = OnError error }, Cmd.none )

        OnQuestionsShuffled questions ->
            case questions of
                first :: otherQuestions ->
                    ( { model | game = Loaded (Game first otherQuestions []) }, Cmd.none )

                _ ->
                    ( { model | route = HomeRoute }, Cmd.none )

        OnCategoriesFetched (Ok categories) ->
            ( { model | categories = Loaded categories }, Cmd.none )

        OnCategoriesFetched (Err err) ->
            ( { model | categories = OnError err }, Cmd.none )

        AnswerQuestion answer ->
            case model.game of
                Loaded game ->
                    let
                        result =
                            if game.currentQuestion.correctAnswer == answer then
                                Correct
                            else
                                Incorrect

                        answeredQuestions =
                            AnsweredQuestion game.currentQuestion result
                                :: game.answeredQuestions
                    in
                        case game.questions of
                            [] ->
                                ( { model | game = Loaded (Game game.currentQuestion game.questions answeredQuestions) }, redirectToResult game )

                            firstRemaining :: others ->
                                ( { model | game = Loaded (Game firstRemaining others answeredQuestions) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


redirectToResult : Game -> Cmd Msg
redirectToResult game =
    game.answeredQuestions
        |> List.map
            (\answeredQuestion ->
                if answeredQuestion.result == Correct then
                    1
                else
                    0
            )
        |> List.sum
        |> toString
        |> (++) "#/result/"
        |> Navigation.newUrl


getCategoriesUrl : String
getCategoriesUrl =
    "https://opentdb.com/api_category.php"


categoriesDecoder : Json.Decode.Decoder (List Category)
categoriesDecoder =
    field "trivia_categories" (map2 Category (field "id" int) (field "name" string) |> list)


getQuestionsUrl : Maybe Int -> String
getQuestionsUrl maybeCategoryId =
    case maybeCategoryId of
        Just categoryId ->
            "https://opentdb.com/api.php?amount=5&type=multiple&category=" ++ (toString categoryId)

        Nothing ->
            "https://opentdb.com/api.php?amount=5&type=multiple"


getCategories : Http.Request (List Category)
getCategories =
    Http.get getCategoriesUrl categoriesDecoder


shuffleQuestions : List Question -> Random.Generator (List Question)
shuffleQuestions =
    List.map shuffleQuestion >> Random.Extra.combine


shuffleQuestion : Question -> Random.Generator Question
shuffleQuestion question =
    Random.map (\shuffledAnswers -> { question | answers = shuffledAnswers }) (Random.List.shuffle question.answers)


getQuestions : Maybe Int -> Http.Request (List Question)
getQuestions categoryIdMaybe =
    Http.get (getQuestionsUrl categoryIdMaybe) questionsDecoder


getQuestionsRequest : Cmd Msg
getQuestionsRequest =
    Http.send OnQuestionsFetched (getQuestions Nothing)


questionsDecoder : Json.Decode.Decoder (List Question)
questionsDecoder =
    field "results" (list questionDecoder)


questionDecoder : Json.Decode.Decoder Question
questionDecoder =
    map3 Question (field "question" string) (field "correct_answer" string) answersDecoder


answersDecoder : Json.Decode.Decoder (List String)
answersDecoder =
    map2 (::) (field "correct_answer" string) (field "incorrect_answers" (list string))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ displayPage model
        ]


displayPage : Model -> Html Msg
displayPage model =
    case model.route of
        HomeRoute ->
            div []
                [ h1 [] [ text "Quiz Game" ]
                , a [ class "btn btn-primary mr-3", href "#game" ] [ text "Play random questions" ]
                , a [ class "btn btn-primary", href "#categories" ] [ text "Play from a category" ]
                ]

        GameRoute categoryIdMaybe ->
            gamePage model categoryIdMaybe

        CategoriesRoute ->
            div []
                [ h1 [] [ text "Play within a given category" ]
                , categoriesPage model.categories
                ]

        ResultRoute score ->
            div [ class "score" ]
                [ p [] [ text ("Your score: " ++ (toString score) ++ " / 5") ]
                , a [ class "btn btn-primary", href "#" ] [ text "Replay" ]
                ]


categoriesPage : RemoteData (List Category) -> Html Msg
categoriesPage categoriesRemote =
    case categoriesRemote of
        Loaded categories ->
            ul [ class "categories" ] (List.map displayCategory categories)

        OnError error ->
            text "An error occurred while fetching categories"

        _ ->
            text "Categories not loaded"


displayCategory : Category -> Html Msg
displayCategory category =
    let
        path =
            "#game/category/" ++ (toString category.id)
    in
        li []
            [ a [ class "btn btn-primary", href path ] [ text category.name ]
            ]


gamePage : Model -> Maybe Int -> Html Msg
gamePage model categoryIdMaybe =
    case model.game of
        Loaded game ->
            displayGame game

        _ ->
            text "Game is loading..."


displayGame : Game -> Html Msg
displayGame game =
    div []
        [ h2 [ class "question" ] [ text (unescape game.currentQuestion.question) ]
        , ul [ class "answers" ] (List.map displayAnswer game.currentQuestion.answers)
        ]


displayAnswer : String -> Html Msg
displayAnswer answer =
    li []
        [ a [ class "btn btn-primary", onClick (AnswerQuestion answer) ] [ text (unescape answer) ]
        ]
