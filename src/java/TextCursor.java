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

/// Methods with an X suffix must have their return value checked!

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

    public String getInput() {
        return input;
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

    public boolean whiteSpace1X() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isWhitespace(input.charAt(pos))) {
            pos++;
        }
        return pos > start;
    }


    public String stringX(String s) {
        int len = s.length();
        String s1 = input.substring(pos, pos + len);
        if (s1.equals(s)) {
            pos = pos + len;
            return s1;
        } else {
            return null;
        }
    }

    /// TODO Bounds checking...
    public boolean charX(char c) {
        char c1 = input.charAt(pos);
        if (c1 == c) {
            pos = pos + 1;
            return true;
        } else {
            return false;
        }
    }

    public String anyCharX() {
        if (pos < input.length()) {
            String c1 = input.substring(pos, pos+1);
            pos++;
            return c1;
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

    /// returns count of chars skipped
    public int skipChar(char c) {
        int start = pos;
        int len = input.length();
        while (pos < len && input.charAt(pos) == c) {
            pos++;
        }
        return pos - start;
    }

    public String manyNotChar(char ch) {
        int start = pos;
        int len = input.length();
        while (pos < len && input.charAt(pos) != ch) {
            pos++;
        }
        return input.substring(start, pos);
    }

    /// returns count of chars skipped
    public int skipNotChar(char c) {
        int start = pos;
        int len = input.length();
        while (pos < len && input.charAt(pos) != c) {
            pos++;
        }
        return pos - start;
    }

    public String manyAlphabetic() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isAlphabetic(input.charAt(pos))) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String manyLetter() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isLetter(input.charAt(pos))) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String manyDigit() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isDigit(input.charAt(pos))) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String manyLowerCase() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isLowerCase(input.charAt(pos))) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String manyUpperCase() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isUpperCase(input.charAt(pos))) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String manyLetterOrDigit() {
        int start = pos;
        int len = input.length();
        while (pos < len && Character.isLetterOrDigit(input.charAt(pos))) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public String manyOneOf(String allowed) {
        int start = pos;
        int len = input.length();
        while (pos < len && (allowed.indexOf(input.charAt(pos)) > -1)) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public int skipOneOf(String allowed) {
        int start = pos;
        int len = input.length();
        while (pos < len && (allowed.indexOf(input.charAt(pos)) > -1)) {
            pos++;
        }
        return pos - start;
    }

    public String manyNoneOf(String notallowed) {
        int start = pos;
        int len = input.length();
        while (pos < len && (notallowed.indexOf(input.charAt(pos)) == -1)) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public int skipNoneOf(String notallowed) {
        int start = pos;
        int len = input.length();
        while (pos < len && (notallowed.indexOf(input.charAt(pos)) == -1)) {
            pos++;
        }
        return pos - start;
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

    public String charsTillString(String needle) {
        int start = pos;
        int len = input.length();
        while (pos < len && (input.startsWith(needle, pos) == false)) {
            pos++;
        }
        return input.substring(start, pos);
    }

    public int skipCharsTillString(String needle) {
        int start = pos;
        int len = input.length();
        while (pos < len && (input.startsWith(needle, pos) == false)) {
            pos++;
        }
        return pos - start;
    }


    public String restOfLine(boolean consumeEol) {
        int start = pos;
        int len = input.length();
        while (pos < len && (input.startsWith("\r\n", pos) == false
                                && input.charAt(pos) != '\n')
                                && input.charAt(pos) != '\r') {
            pos++;
        }
        if (consumeEol && pos < len) {
            if (input.startsWith("\r\n", pos)) {
                pos = pos + 2;
            } else if (input.charAt(pos) == '\n') {
                pos++;
            } else if (input.charAt(pos) == '\r') {
                pos++;
            }
        }
        return input.substring(start, pos);
    }

    public int skipRestOfLine(boolean consumeEol) {
        int start = pos;
        int len = input.length();
        while (pos < len && (input.startsWith("\r\n", pos) == false
                                && input.charAt(pos) != '\n'
                                && input.charAt(pos) != '\r')) {
            pos++;
        }
        if (consumeEol && pos < len) {
            if (input.startsWith("\r\n", pos)) {
                pos = pos + 2;
            } else if (input.charAt(pos) == '\n') {
                pos++;
            } else if (input.charAt(pos) == '\r') {
                pos++;
            }
        }
        return pos - start;
    }

    public boolean isEof() {
        return pos >= input.length();
    }
}
