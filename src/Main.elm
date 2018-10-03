module Main exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import MD5
import String exposing (isEmpty)



---- MODEL ----


type User
    = Anonymous
    | User UserData


type alias UserData =
    String


type alias Model =
    { login : String
    , password : String
    , hashedPassword : String
    , user : User
    , message : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { login = ""
      , password = ""
      , hashedPassword = ""
      , user = Anonymous
      , message = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Login String
    | Password String
    | OnLogin
    | Resp (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login login ->
            ( { model
                | login = login
              }
            , Cmd.none
            )

        Password password ->
            ( { model
                | password = password
              }
            , Cmd.none
            )

        OnLogin ->
            let
                hash =
                    MD5.hex model.password
            in
            ( { model | hashedPassword = hash }
            , req model.login hash
            )

        Resp result ->
            case result of
                Ok message ->
                    ( { model | message = message, user = User model.login }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.user of
        Anonymous ->
            div
                [ css
                    [ backgroundColor (hex "293c4b")
                    , displayFlex
                    , flexDirection column
                    , justifyContent center
                    , alignItems center
                    , minHeight (vh 100)
                    ]
                ]
                [ viewInput "text" "Name" model.login Login
                , viewInput "password" "Password" model.password Password

                -- , button [ onClick OnLogin, attribute "disabled" (enableButton model) ] [ text "Login" ]
                , button
                    [ onClick OnLogin
                    , attribute "disabled"
                        ((\{ login, password } ->
                            if isEmpty login || isEmpty password then
                                "true"

                            else
                                ""
                         )
                            model
                        )
                    ]
                    [ text "Login" ]
                ]

        User userName ->
            div [] [ text userName ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- MAIN ----


main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


req : String -> String -> Cmd Msg
req login hashedPassword =
    Http.send Resp
        (Http.request
            { body =
                Http.multipartBody
                    [ Http.stringPart "login" login
                    , Http.stringPart "hashedPassword" hashedPassword
                    ]
            , expect = Http.expectJson postDecoder
            , headers = []
            , method = "POST"
            , timeout = Nothing
            , url = "http://127.0.0.1:3000/login"
            , withCredentials = False
            }
        )


enableButton : Model -> String
enableButton { login, password } =
    if isEmpty login || isEmpty password then
        "true"

    else
        ""


postDecoder : Decode.Decoder String
postDecoder =
    Decode.string
