module Move exposing (PossibleMoves, commit, emptyPossibleMoves, generatePossibleMoves, possible)

import Array exposing (Array)
import Board exposing (Board, Piece, PieceType(..), Player(..), Position)
import Tuple.Extra


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


markPossibleMoves : List Position -> PossibleMoves
markPossibleMoves positions =
    List.foldl markPossibleMove emptyPossibleMoves positions


markPossibleMoveIfEmpty : Board -> Position -> PossibleMoves -> PossibleMoves
markPossibleMoveIfEmpty board position possibleMoves =
    if Board.isEmpty board position then
        markPossibleMove position possibleMoves

    else
        possibleMoves


markPossibleMoveIfEnemy : Board -> Player -> Position -> PossibleMoves -> PossibleMoves
markPossibleMoveIfEnemy board turn position possibleMoves =
    if Board.isEnemy board position turn then
        markPossibleMove position possibleMoves

    else
        possibleMoves


{-| "Ray" is a line that starts at a position and moves indefinitely in one direction.
Delta is the difference in position from one board square along the line to the next board square
-}
moveAlongRay : Board -> Position -> Position -> Player -> PossibleMoves -> PossibleMoves
moveAlongRay board delta position player possibleMoves =
    let
        newPosition =
            Tuple.Extra.add delta position

        nextIsValid =
            Board.validPosition newPosition && Board.isNotSelf board newPosition player

        currentIsEnemy =
            Board.isEnemy board position player
    in
    if currentIsEnemy || not nextIsValid then
        possibleMoves

    else
        markPossibleMove newPosition possibleMoves
            |> moveAlongRay board delta newPosition player


{-| Deltas is a list of differences. See moveALongRay comment.
-}
moveAlongRays : Board -> List Position -> Position -> Player -> PossibleMoves
moveAlongRays board deltas position player =
    let
        moveAlong rayDelta =
            moveAlongRay board rayDelta position player
    in
    List.foldl moveAlong emptyPossibleMoves deltas


generatePossibleMoves : Board -> Position -> PieceType -> Player -> PossibleMoves
generatePossibleMoves board position pieceType turn =
    let
        ( fromColumn, fromRow ) =
            position

        facing =
            case turn of
                White ->
                    1

                Black ->
                    -1

        diagonals =
            [ ( 1, 1 ), ( 1, -1 ), ( -1, 1 ), ( -1, -1 ) ]

        filesAndRanks =
            [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ), ( 0, -1 ) ]

        anyDirection =
            List.append diagonals filesAndRanks

        moveAlong deltas =
            moveAlongRays board deltas position turn

        landings =
            List.map (Tuple.Extra.add position)
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
            let
                elles =
                    [ ( 2, 1 ), ( 2, -1 ), ( -2, 1 ), ( -2, -1 ), ( 1, 2 ), ( 1, -2 ), ( -1, 2 ), ( -1, -2 ) ]

                isValid landing =
                    Board.validPosition landing && Board.isNotSelf board landing turn
            in
            elles
                |> landings
                |> List.filter isValid
                |> markPossibleMoves

        Bishop ->
            moveAlong diagonals

        Rook ->
            moveAlong filesAndRanks

        Queen ->
            moveAlong anyDirection

        King ->
            let
                isValid landing =
                    -- TODO: This doesn't check for getting yourself into check
                    Board.validPosition landing && Board.isNotSelf board landing turn
            in
            anyDirection |> landings |> List.filter isValid |> markPossibleMoves


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
