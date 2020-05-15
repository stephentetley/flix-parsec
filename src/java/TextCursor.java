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

package flix.runtime.spt.textparser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TextCursor {

    String input;
    int pos = 0;

    public TextCursor(String src) {
        input = src;
        pos = 0;
    }

    public int getPos() {
        return pos;
    }

    public void setPos(int x) {
        pos = x;
    }

    public void skipWhiteSpace() {
        int len = input.length();
        while (pos < len && Character.isWhitespace(input.charAt(pos))) {
            pos++;
        }
    }

    public String whiteSpace() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isWhitespace(input.charAt(pos))) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String literalX(String s) {
        int len = s.length();
        String s1 = input.substring(pos, pos + len);
        if (s1.equals(s)) {
            pos = pos + len;
            return s1;
        } else {
            return null;
        }
    }

    public String manyChar(char c) {
        int start = pos;
        int len = input.length();
        while (pos < len && input.charAt(pos) == c) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String many1CharX(char c) {
        int start = pos;
        int len = input.length();
        while (pos < len && input.charAt(pos) == c) {
            pos++;
        }
        if (pos > start) {
            return input.substring(start, pos);
        } else {
            return null;
        }
    }


    public String lookingAtX(Pattern p1) {
        Matcher m1 = p1.matcher(input);
        m1.region(pos, input.length());
        if (m1.lookingAt()) {
            String ans = m1.group();
            pos = pos + ans.length();
            return ans;
        } else {
            return null;
        }
    }

}
