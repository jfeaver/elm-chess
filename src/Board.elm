module Board exposing (..)

import Array exposing (Array)


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


notPlayer : Player -> Player
notPlayer player =
    case player of
        White ->
            Black

        Black ->
            White


nextTurn =
    notPlayer


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


type alias BoardSquare =
    Maybe Piece


type alias Column =
    Array BoardSquare


type alias Board =
    -- [column][row]
    Array Column


white : PieceType -> Piece
white pieceType =
    Piece White pieceType


black : PieceType -> Piece
black pieceType =
    Piece Black pieceType


backRow =
    [ Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook ] |> Array.fromList


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


new : Board
new =
    let
        rowInitializer columnN rowN =
            case rowN of
                0 ->
                    Array.get columnN row0

                1 ->
                    Array.get columnN row1

                6 ->
                    Array.get columnN row6

                7 ->
                    Array.get columnN row7

                _ ->
                    Nothing

        columnInitializer columnN =
            Array.initialize 8 (rowInitializer columnN)
    in
    Array.initialize 8 columnInitializer


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


get : Position -> Board -> BoardSquare
get ( column, row ) board =
    Array.get column board
        |> Maybe.andThen (Array.get row)
        |> Maybe.andThen identity


isKing : Position -> Board -> Bool
isKing position board =
    get position board
        |> Maybe.map (\piece -> piece.pieceType == King)
        |> Maybe.withDefault False
