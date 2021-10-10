module Move exposing (PossibleMoves, commit, emptyPossibleMoves, generatePossibleMoves, possible)

import Array exposing (Array)
import Board exposing (Board, Piece, PieceType(..), Player(..), Position)


type alias PossibleMove =
    Bool


type alias PossibleMoves =
    Array (Array PossibleMove)


emptyPossibleMoves : PossibleMoves
emptyPossibleMoves =
    Array.repeat 8 (Array.repeat 8 False)


possible : Position -> PossibleMoves -> PossibleMove
possible ( column, row ) possibleMoves =
    Array.get column possibleMoves
        |> Maybe.andThen (Array.get row)
        |> Maybe.withDefault False


markPossibleMove : Position -> PossibleMoves -> PossibleMoves
markPossibleMove ( column, row ) possibleMoves =
    case Array.get column possibleMoves of
        Just columnArray ->
            let
                newColumn =
                    Array.set row True columnArray
            in
            Array.set column newColumn possibleMoves

        Nothing ->
            possibleMoves


markPossibleMoveIfEmpty : Board -> Position -> PossibleMoves -> PossibleMoves
markPossibleMoveIfEmpty board position possibleMoves =
    case Board.get position board of
        Just _ ->
            possibleMoves

        Nothing ->
            markPossibleMove position possibleMoves


markPossibleMoveIfEnemy : Board -> Player -> Position -> PossibleMoves -> PossibleMoves
markPossibleMoveIfEnemy board turn position possibleMoves =
    case Board.get position board of
        Just { player } ->
            if player == Board.notPlayer turn then
                markPossibleMove position possibleMoves

            else
                possibleMoves

        Nothing ->
            possibleMoves


generatePossibleMoves : Board -> Position -> PieceType -> Player -> PossibleMoves
generatePossibleMoves board ( fromColumn, fromRow ) pieceType turn =
    let
        facing =
            case turn of
                White ->
                    1

                Black ->
                    -1
    in
    case pieceType of
        Pawn ->
            let
                forwardTwo =
                    if fromRow == 1 && turn == White || fromRow == 6 && turn == Black then
                        emptyPossibleMoves
                            |> markPossibleMoveIfEmpty board ( fromColumn, fromRow + 2 * facing )

                    else
                        emptyPossibleMoves

                forwardMoves =
                    forwardTwo
                        |> markPossibleMoveIfEmpty board ( fromColumn, fromRow + facing )

                withAttacks =
                    forwardMoves
                        |> markPossibleMoveIfEnemy board turn ( fromColumn + 1, fromRow + facing )
                        |> markPossibleMoveIfEnemy board turn ( fromColumn - 1, fromRow + facing )
            in
            withAttacks

        Knight ->
            emptyPossibleMoves

        Bishop ->
            emptyPossibleMoves

        Rook ->
            emptyPossibleMoves

        Queen ->
            emptyPossibleMoves

        King ->
            emptyPossibleMoves


placePiece : Position -> Maybe Piece -> Board -> Board
placePiece ( column, row ) maybePiece board =
    case Array.get column board of
        Just columnArray ->
            let
                newColumn =
                    Array.set row maybePiece columnArray
            in
            Array.set column newColumn board

        Nothing ->
            board


commit : Position -> Position -> Board -> Board
commit ( fromColumn, fromRow ) toPosition board =
    Array.get fromColumn board
        |> Maybe.andThen (Array.get fromRow)
        |> Maybe.map (\mPiece -> placePiece toPosition mPiece board |> placePiece ( fromColumn, fromRow ) Nothing)
        |> Maybe.withDefault board



--facing =
--    case model.turn of
--        White ->
--            1
--
--        Black ->
--            -1
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
