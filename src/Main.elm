module Main exposing (..)

import Array exposing (Array)
import Board exposing (..)
import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Move exposing (PossibleMoves)
import String exposing (fromInt)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type GameState
    = Viewing
    | OnPiece Board.Position PieceType


type alias Game =
    { turn : Player
    , state : GameState
    , board : Board
    , possibleMoves : PossibleMoves
    }


type alias Model =
    Game


newModel : Model
newModel =
    { turn = White
    , state = Viewing
    , board = Board.new
    , possibleMoves = Move.emptyPossibleMoves
    }


init : () -> ( Model, Cmd Msg )
init =
    always ( newModel, Cmd.none )


type Msg
    = StartMove Board.Position PieceType
    | CommitMove Board.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartMove position pieceType ->
            ( { model
                | possibleMoves = Move.generatePossibleMoves model.board position pieceType model.turn
                , state = OnPiece position pieceType
              }
            , Cmd.none
            )

        CommitMove toPosition ->
            let
                updatedBoard =
                    case model.state of
                        Viewing ->
                            model.board

                        OnPiece fromPosition _ ->
                            Move.commit fromPosition toPosition model.board
            in
            ( { model
                | board = updatedBoard
                , possibleMoves = Move.emptyPossibleMoves
                , state = Viewing
                , turn = nextTurn model.turn
              }
            , Cmd.none
            )


pieceView : Model -> Board.Position -> Piece -> Html Msg
pieceView model position piece =
    let
        description =
            (pieceDescription >> (++) " ") piece

        baseCss =
            [ fontSize (Css.em 3.5) ]

        attributes =
            if model.turn == piece.player then
                [ css <| List.append baseCss [ cursor pointer ], onClick (StartMove position piece.pieceType) ]

            else
                [ css baseCss ]
    in
    span attributes [ text description ]


squareView : Model -> Int -> Int -> BoardSquare -> Html Msg
squareView model column row mPiece =
    let
        position =
            fromInt column ++ ":" ++ fromInt row

        background =
            if modBy 2 (column + row) == 0 then
                rgb 180 180 180

            else
                rgb 250 250 250

        squareLength =
            px 60

        positionHelper =
            span [ css [ float left, Css.position absolute, top (px 0), left (px 0) ] ] [ text position ]

        pieceHtml =
            Maybe.map (\piece -> pieceView model ( column, row ) piece) mPiece |> Maybe.withDefault (text "")

        baseCss =
            [ backgroundColor background
            , height squareLength
            , width squareLength
            , Css.position relative
            , displayFlex
            , alignItems center
            , justifyContent center
            ]

        possibleMove =
            Move.possible ( column, row ) model.possibleMoves

        squareCss =
            if possibleMove then
                List.append baseCss
                    [ border3 (px 3) solid (rgb 0 256 0)
                    , cursor pointer
                    ]

            else
                baseCss

        baseAttributes =
            [ css squareCss ]

        attributes =
            if possibleMove then
                (onClick <| CommitMove ( column, row )) :: baseAttributes

            else
                baseAttributes
    in
    div attributes [ positionHelper, pieceHtml ]


boardColumnView : Model -> ( Int, Column ) -> List (Html Msg) -> List (Html Msg)
boardColumnView model ( columnN, column ) html =
    div
        [ css
            [ displayFlex
            , flexDirection columnReverse
            ]
        ]
        (List.indexedMap (squareView model columnN) (Array.toList column))
        :: html


view : Model -> Document Msg
view model =
    let
        turnMessage =
            playerToString model.turn ++ " to move"

        boardHtml =
            List.foldr
                (boardColumnView model)
                []
                (Array.toIndexedList model.board)
    in
    { title = "Elm Chess"
    , body =
        [ div
            []
            [ div [] [ text turnMessage ]
            , div
                [ css
                    [ displayFlex ]
                ]
                boardHtml
            ]
            |> toUnstyled
        ]
    }
