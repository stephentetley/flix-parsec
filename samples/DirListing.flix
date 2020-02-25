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

/// Parser for PowerShell `dir` output, not yet recursive
/// Note - PowerShell ouput is commonly UTF16_LE

type alias TimeStamp = { day: Int32, month: Int32, year: Int32, hour: Int32, minute: Int32 }

/// Don't care about attributes for the moment 

type alias File = { name: String, size: Int64, modified: TimeStamp }
type alias Folder = { name: String, modified: TimeStamp }

namespace DirListing {

    
    pub def printTimeStamp(ts: TimeStamp): String = 
        "${ Int32.toString(ts.day)}/${Int32.toString(ts.month)}/${ Int32.toString(ts.year)} ${ Int32.toString(ts.hour)}:${Int32.toString(ts.minute)}"

    pub def pUkTimeStamp(): SmallParser.Parser[TimeStamp] = 
        SmallParser.pipe5(
            SmallParser.seqLeft(SmallParser.int32(), SmallParser.char('/')),
            SmallParser.seqLeft(SmallParser.int32(), SmallParser.char('/')),
            SmallParser.int32(),
            SmallParser.seqLeft(SmallParser.int32(), SmallParser.char(':')),
            SmallParser.int32(),
            (d, mon, y, h, min) -> { day=d, month=mon, year=y, hour=h, minute=min }
        )

    pub def fileStats(): SmallParser.Parser[(TimeStamp, Int64)] = 
        SmallParser.tuple2(
            pUkTimeStamp(),
            SmallParser.int64()
        )
    
    /// Note - Windows Dir Listing can run to muliple lines - not handled yet...
    pub def fileName(): SmallParser.Parser[String] = 
        SmallParser.fmap(String.trim, SmallParser.restOfLine())

    pub def pFile(): SmallParser.Parser[File] = 
        SmallParser.pipe3(
            SmallParser.horizon(13),
            SmallParser.bounded(fileStats(), 36),
            fileName(),
            (_, x, name) -> { 
                let (ts, size) = x; 
                { name=name, size=size, modified=ts } 
            }
        )
    pub def pFolder(): SmallParser.Parser[Folder] = 
        SmallParser.pipe3(
            SmallParser.horizon(13),
            SmallParser.bounded(pUkTimeStamp(), 36),
            fileName(),
            (_, ts, name) -> { name=name, modified=ts }
        )

    pub def ignoreLine(): SmallParser.Parser[Option[a]] = 
        SmallParser.fmap(_ -> None, SmallParser.restOfLine())

}