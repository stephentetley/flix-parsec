/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use System.Error.{IllegalArgumentException, InputMismatchException};

use TextParser.TextParser;
use TextParser.RowParser;
use TextParser.RowParser.{RowParser};

namespace TextParser {

    pub opaque type RowParser[a] = Array[String] ~> Result[a, System.Error]
}

namespace TextParser/RowParser {

    pub def rowTextParser(splitter: String ~> Array[String], rowp: RowParser[a]): TextParser[a, t] =
        use TextParser.{flatMap, return, liftAction, liftActionResult};
        use TextParser/Text.{restOfLine};
        let* s0 = restOfLine(true);
        let s1 = Text/Regex.replace(s0, Text/Regex/Pattern.irrefutablePattern("\R"), "");
        let* arr = liftAction(_ -> splitter(s1));
        let* ans = liftActionResult(_ -> apply1(rowp, arr));
        return(ans)

    pub def return(x: a): RowParser[a] =
        RowParser(_ -> Ok(x) as & Impure)

    def apply1(x: RowParser[a], row: Array[String]): Result[a, System.Error] & Impure =
        let RowParser(f) = x;
        f(row)

    pub def map(f: a -> b & ef, x: RowParser[a]): RowParser[b] =
        RowParser(row ->
            match apply1(x, row) {
                case Ok(a) => {
                    let ans = f(a) as & Impure;
                    Ok(ans)
                }
                case Err(e) => Err(e)
            }
        )        

    pub def ap(mf: RowParser[a -> b & ef], ma: RowParser[a]): RowParser[b] =
        RowParser(row ->
            match apply1(mf, row) {
                case Ok(f) => { 
                    match apply1(ma, row) {
                        case Ok(a) => Ok(f(a) as & Impure)
                        case Err(e2) => Err(e2)
                    }
                }
                case Err(e1) => Err(e1)
            }
        )

    pub def bind(x: RowParser[a], k: a -> RowParser[b]): RowParser[b] =
        RowParser(row ->
            match apply1(x, row) {
                case Ok(a) => apply1(k(a), row)
                case Err(e) => Err(e)
            }
        )

    pub def flatMap(k: a -> RowParser[b], x: RowParser[a]): RowParser[b] = bind(x, k)

    pub def optional(x: RowParser[a]): RowParser[Option[a]] =
        RowParser(r ->
            match apply1(x, r) {
                case Ok(a) => Ok(Some(a))
                case Err(_) => Ok(None)
            }
        )

    pub def liftResult(x: Result[a, System.Error]): RowParser[a] =
        RowParser(_ -> x as & Impure)

    pub def liftOperation(fn: Unit ~> Result[a, System.Error]): RowParser[a] =
        RowParser(_ -> fn())

    // Cell readers, by index only

    /// Note - answer is trimmed, use `getCell` if you need exact contents
    pub def getString(ix: Int32): RowParser[String] =
        map(String.trim, getCell(ix))

    pub def getCell(ix: Int32): RowParser[String] =
        RowParser(row ->
            if (ix >= 0 && ix < Array.length(row)) {
                let s = row[ix];
                Ok(s)
            } else {
                Err(IllegalArgumentException)
            }
        )


    pub def evalMatches(ix: Int32, patt: Text/Regex.Pattern, eval: Text/Regex.MatchEvaluator[a]): RowParser[a] =
        let* input = getString(ix);
        let* ans = liftOperation(_ -> Text/Regex.evalMatches(input, patt, eval));
        return(ans)

    pub def evalContains(ix: Int32, patt: Text/Regex.Pattern, eval: Text/Regex.MatchEvaluator[a]): RowParser[a] =
        let* input = getString(ix);
        let* ans = liftOperation(_ -> Text/Regex.evalContains(input, patt, eval));
        return(ans)

    pub def getInt8(ix: Int32): RowParser[Int8] =
        let* s = getString(ix);
        liftResult(Int8.fromString(s) |> Result.mapErr(_ -> InputMismatchException))
        
    pub def getInt16(ix: Int32): RowParser[Int16] =
        let* s = getString(ix);
        liftResult(Int16.fromString(s) |> Result.mapErr(_ -> InputMismatchException))
    
    pub def getInt32(ix: Int32): RowParser[Int32] =
        let* s = getString(ix);
        liftResult(Int32.fromString(s) |> Result.mapErr(_ -> InputMismatchException))
    
    pub def getInt64(ix: Int32): RowParser[Int64] =
        let* s = getString(ix);
        liftResult(Int64.fromString(s) |> Result.mapErr(_ -> InputMismatchException))

    pub def getFloat32(ix: Int32): RowParser[Float32] =
        let* s = getString(ix);
        liftResult(Float32.fromString(s) |> Result.mapErr(_ -> InputMismatchException))

    pub def getFloat64(ix: Int32): RowParser[Float64] =
        let* s = getString(ix);
        liftResult(Float64.fromString(s) |> Result.mapErr(_ -> InputMismatchException))

    pub def getLocalDate(ix: Int32, formatter: Time/Format.DateFormatter): RowParser[Time.LocalDate] =
        let* input = getString(ix);
        let* ans = liftResult(Time/LocalDate.parse(input, formatter));
        return(ans)


    pub def getLocalDateTime(ix: Int32, formatter: Time/Format.DateTimeFormatter): RowParser[Time.LocalDateTime] =
        let* input = getString(ix);
        let* ans = liftResult(Time/LocalDateTime.parse(input, formatter));
        return(ans)


    pub def getLocalTime(ix: Int32, formatter: Time/Format.TimeFormatter): RowParser[Time.LocalTime] =
        let* input = getString(ix);
        let* ans = liftResult(Time/LocalTime.parse(input, formatter));
        return(ans)

    ///
    /// Applies the binary function `f` to the values in `o1` and `o2`.
    ///
    /// Returns an error if either `o1` or `o2` are errors.
    ///
    pub def lift2(f: (t1, t2) -> u & ef, o1: RowParser[t1], o2: RowParser[t2]): RowParser[u] =
        ap(map(f, o1), o2)

    ///
    /// Applies the ternary function `f` to the values in `o1`, `o2` and `o3`.
    ///
    /// Returns an error if any of `o1`, `o2` and `o3` are errors.
    ///
    pub def lift3(f: (t1, t2, t3) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3]): RowParser[u] =
        ap(lift2(f, o1, o2), o3)

    ///
    /// Applies the 4-ary function `f` to the values in `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns an error if any of `o1`, `o2`, `o3` and `o4` are errors.
    ///
    pub def lift4(f: (t1, t2, t3, t4) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4]): RowParser[u] =
        ap(lift3(f, o1, o2, o3), o4)

    ///
    /// Applies the 5-ary function `f` to the values in `o1`, `o2`, ... `o5`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o5` are errors.
    ///
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5]): RowParser[u] =
        ap(lift4(f, o1, o2, o3, o4), o5)

    ///
    /// Applies the 6-ary function `f` to the values in `o1`, `o2`, ... `o6`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o6` are errors.
    ///
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6]): RowParser[u] =
        ap(lift5(f, o1, o2, o3, o4, o5), o6)

    ///
    /// Applies the 7-ary function `f` to the values in `o1`, `o2`, ... `o7`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o7` are errors.
    ///
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7]): RowParser[u] =
        ap(lift6(f, o1, o2, o3, o4, o5, o6), o7)

    ///
    /// Applies the 8-ary function `f` to the values in `o1`, `o2`, ... `o8`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o8` are errors.
    ///
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7], o8: RowParser[t8]): RowParser[u] =
        ap(lift7(f, o1, o2, o3, o4, o5, o6, o7), o8)

    ///
    /// Applies the 9-ary function `f` to the values in `o1`, `o2`, ... `o9`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o9` are errors.
    ///
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7], o8: RowParser[t8], o9: RowParser[t9]): RowParser[u] =
        ap(lift8(f, o1, o2, o3, o4, o5, o6, o7, o8), o9)

    ///
    /// Applies the 10-ary function `f` to the values in `o1`, `o2`, ... `o10`.
    ///
    /// Returns an error if any of `o1`, `o2`, ... `o10` are errors.
    ///
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u & ef, o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7], o8: RowParser[t8], o9: RowParser[t9], o10: RowParser[t10]): RowParser[u] =
        ap(lift9(f, o1, o2, o3, o4, o5, o6, o7, o8, o9), o10)

    ///
    /// Returns a 2 element tuple with the results of applying the parsers `o1` and `o2`.
    ///
    /// Returns an error if evaluating either `o1` or `o2` is an error.
    ///
    pub def tuple2(o1: RowParser[t1], o2: RowParser[t2]): RowParser[(t1, t2)] =
        lift2((x,y) -> (x,y), o1, o2)

    ///
    /// Returns a 3 element tuple with the results of applying the parsers `o1`, `o2` and `o3`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2` and `o3` is an error.
    ///
    pub def tuple3(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3]): RowParser[(t1, t2, t3)] =
        lift3((x1, x2, x3) -> (x1, x2, x3), o1, o2, o3)

    ///
    /// Returns a 4 element tuple with the results of applying the parsers `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, `o3` and `o4` is an error.
    ///
    pub def tuple4(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4]): RowParser[(t1, t2, t3, t4)]  =
        lift4((x1, x2, x3, x4) -> (x1, x2, x3, x4), o1, o2, o3, o4)

    ///
    /// Returns a 5 element tuple with the results of applying the parsers `o1`, `o2`, ... `o5`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o5` is an error.
    ///
    pub def tuple5(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5]): RowParser[(t1, t2, t3, t4, t5)]  =
        lift5((x1, x2, x3, x4, x5) -> (x1, x2, x3, x4, x5), o1, o2, o3, o4, o5)

    ///
    /// Returns a 6 element tuple with the results of applying the parsers `o1`, `o2`, ... `o6`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o6` is an error.
    ///
    pub def tuple6(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6]): RowParser[(t1, t2, t3, t4, t5, t6)]  =
        lift6((x1, x2, x3, x4, x5, x6) -> (x1, x2, x3, x4, x5, x6), o1, o2, o3, o4, o5, o6)

    ///
    /// Returns a 7 element tuple with the results of applying the parsers `o1`, `o2`, ... `o7`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o7` is an error.
    ///
    pub def tuple7(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7]): RowParser[(t1, t2, t3, t4, t5, t6, t7)]  =
        lift7((x1, x2, x3, x4, x5, x6, x7) -> (x1, x2, x3, x4, x5, x6, x7), o1, o2, o3, o4, o5, o6, o7)

    ///
    /// Returns a 8 element tuple with the results of applying the parsers `o1`, `o2`, ... `o8`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o8` is an error.
    ///
    pub def tuple8(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7], o8: RowParser[t8]): RowParser[(t1, t2, t3, t4, t5, t6, t7, t8)]  =
        lift8((x1, x2, x3, x4, x5, x6, x7, x8) -> (x1, x2, x3, x4, x5, x6, x7, x8), o1, o2, o3, o4, o5, o6, o7, o8)

    ///
    /// Returns a 9 element tuple with the results of applying the parsers `o1`, `o2`, ... `o9`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o9` is an error.
    ///
    pub def tuple9(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7], o8: RowParser[t8], o9: RowParser[t9]): RowParser[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]  =
        lift9((x1, x2, x3, x4, x5, x6, x7, x8, x9) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9), o1, o2, o3, o4, o5, o6, o7, o8, o9)

    ///
    /// Returns a 10 element tuple with the results of applying the parsers `o1`, `o2`, ... `o10`.
    ///
    /// Returns an error if evaluating any of `o1`, `o2`, ... `o10` is an error.
    ///
    pub def tuple10(o1: RowParser[t1], o2: RowParser[t2], o3: RowParser[t3], o4: RowParser[t4], o5: RowParser[t5], o6: RowParser[t6], o7: RowParser[t7], o8: RowParser[t8], o9: RowParser[t9], o10: RowParser[t10]): RowParser[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]  =
        lift10((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), o1, o2, o3, o4, o5, o6, o7, o8, o9, o10)

}