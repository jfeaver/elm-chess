module Main exposing (..)

import Array
import Browser exposing (Document)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import String exposing (fromInt)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Position =
    ( Int, Int )


type Player
    = White
    | Black


playerToString : Player -> String
playerToString player =
    case player of
        White ->
            "White"

        Black ->
            "Black"


type PieceType
    = Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King


type alias Piece =
    { player : Player
    , pieceType : PieceType
    }


type GameState
    = Viewing
    | OnPiece Position PieceType


type alias PossibleMove =
    Bool


type alias BoardSquare =
    ( Maybe Piece, PossibleMove )


type alias NumberedColumn =
    ( Int, List BoardSquare )


type alias Board =
    -- TODO: Board probably needs to be made of arrays so that I can more easily index things
    -- [inverseColumn][row]
    List NumberedColumn


type alias Game =
    { turn : Player
    , state : GameState
    , board : Board
    }


type alias Model =
    Game


white : PieceType -> Piece
white pieceType =
    Piece White pieceType


black : PieceType -> Piece
black pieceType =
    Piece Black pieceType


backRow =
    [ Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook ] |> Array.fromList


frontRow =
    Array.repeat 8 Pawn


row0 =
    Array.map white backRow


row1 =
    Array.map white frontRow


row6 =
    Array.map black frontRow


row7 =
    Array.map black backRow


newBoard : Board
newBoard =
    let
        columnBuilder columnN () =
            ( columnN
            , [ ( Array.get columnN row0, False )
              , ( Array.get columnN row1, False )
              , ( Nothing, False )
              , ( Nothing, False )
              , ( Nothing, False )
              , ( Nothing, False )
              , ( Array.get columnN row6, False )
              , ( Array.get columnN row7, False )
              ]
            )
    in
    List.repeat 8 ()
        |> List.indexedMap columnBuilder


newModel : Model
newModel =
    { turn = White
    , state = Viewing
    , board = newBoard
    }


init : () -> ( Model, Cmd Msg )
init =
    always ( newModel, Cmd.none )


type Msg
    = StartMove Position PieceType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartMove position pieceType ->
            -- TODO: update model board with marked possible moves
            ( { model | state = OnPiece position pieceType }, Cmd.none )


pieceDescription : Piece -> String
pieceDescription { player, pieceType } =
    case pieceType of
        Pawn ->
            case player of
                White ->
                    "♙"

                Black ->
                    "♟"

        Knight ->
            case player of
                White ->
                    "♘"

                Black ->
                    "♞"

        Bishop ->
            case player of
                White ->
                    "♗"

                Black ->
                    "♝"

        Rook ->
            case player of
                White ->
                    "♖"

                Black ->
                    "♜"

        Queen ->
            case player of
                White ->
                    "♕"

                Black ->
                    "♛"

        King ->
            case player of
                White ->
                    "♔"

                Black ->
                    "♚"


pieceView : Model -> Position -> Piece -> Html Msg
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
squareView model column row ( mPiece, possibleMove ) =
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

        --facing =
        --    case model.turn of
        --        White ->
        --            1
        --
        --        Black ->
        --            -1
        --possibleMove =
        --    False
        --case model.state of
        --    ViewingGameState ->
        --        False
        --
        --    OnPiece ( fromColumn, fromRow ) pieceType ->
        --        let
        --            up1 =
        --                fromRow + 1 == row
        --
        --            up2 =
        --                fromRow + 2 == row
        --
        --            back1 =
        --                fromRow - 1 == row
        --
        --            back2 =
        --                fromRow - 2 == row
        --
        --            left1 =
        --                fromColumn - 1 == column
        --
        --            left2 =
        --                fromColumn - 2 == column
        --
        --            right1 =
        --                fromColumn + 1 == column
        --
        --            right2 =
        --                fromColumn + 2 == column
        --
        --            diagonal =
        --                fromRow - row == fromColumn - column || fromRow - row == fromColumn + column || fromRow + row == fromColumn - column || fromRow + row == fromColumn + column
        --        in
        --        case pieceType of
        --            Pawn ->
        --                fromRow + facing == row && fromColumn == column
        --
        --            Knight ->
        --                up1 && left2 || up1 && right2 || up2 && left1 || up2 && right1 || back1 && left2 || back1 && right2 || back2 && left1 || back2 && right1
        --
        --            Bishop ->
        --                diagonal
        --
        --            Rook ->
        --                False
        --
        --            Queen ->
        --                False
        --
        --            King ->
        --                False
        squareCss =
            if possibleMove then
                border3 (px 3) solid (rgb 0 256 0) :: baseCss

            else
                baseCss
    in
    div [ css squareCss ] [ positionHelper, pieceHtml ]


boardColumnView : Model -> NumberedColumn -> List (Html Msg) -> List (Html Msg)
boardColumnView model ( invertColumnN, column ) html =
    div
        [ css
            [ displayFlex
            , flexDirection columnReverse
            ]
        ]
        (List.indexedMap (squareView model (7 - invertColumnN)) column)
        :: html


view : Model -> Document Msg
view model =
    let
        turnMessage =
            playerToString model.turn ++ " to move"

        boardHtml =
            List.foldl
                (boardColumnView model)
                []
                model.board
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
