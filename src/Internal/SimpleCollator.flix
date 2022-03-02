/*
 * Copyright 2021 Stephen Tetley
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

/// Note
/// This a substitute for the Text/Collator module in `flix-sandbox` that 
/// has a larger API and allows the collator to be customized

namespace FlixParsec/Internal/SimpleCollator {

    /// Compare with the default collator (this will be locale specific).
    pub def caseInsenstiveEquals(s1: String, s2: String): Bool = caseInsenstiveEqualsHelper(s1, s2) as & Pure 

    def caseInsenstiveEqualsHelper(s1: String, s2: String): Bool & Impure = 
        import static java.text.Collator.getInstance(): ##java.text.Collator & Impure;
        import java.text.Collator.equals(String, String): Bool & Pure;
        let collator = getInstance();
        equals(collator, s1, s2)

}
