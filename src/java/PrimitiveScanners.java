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

import java.lang.String;

public class PrimitiveScanners {

    public static String manyAlphabetic(String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && Character.isAlphabetic(src.charAt(pos))) {
            pos++;
        }
        return src.substring(offset, pos);
    }

    public static int skipAlphabetic(String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && Character.isAlphabetic(src.charAt(pos))) {
            pos++;
        }
        return pos;
    }

    public static String manyChars(char ch, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && src.charAt(pos) == ch) {
            pos++;
        }
        return src.substring(offset, pos);
    }

    public static int skipChars(char ch, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && src.charAt(pos) == ch) {
            pos++;
        }
        return pos;
    }

    public static String manyNotChar(char ch, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && src.charAt(pos) != ch) {
            pos++;
        }
        return src.substring(offset, pos);
    }

    public static int skipNotChar(char ch, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && src.charAt(pos) != ch) {
            pos++;
        }
        return pos;
    }

    public static String manyOneOf(String allowed, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (allowed.indexOf(src.charAt(pos)) > -1)) {
            pos++;
        }
        return src.substring(offset, pos);
    }

    public static int skipOneOf(String allowed, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (allowed.indexOf(src.charAt(pos)) > -1)) {
            pos++;
        }
        return pos;
    }

    public static String manyNoneOf(String notallowed, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (notallowed.indexOf(src.charAt(pos)) == -1)) {
            pos++;
        }
        return src.substring(offset, pos);
    }

    public static int skipNoneOf(String notallowed, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (notallowed.indexOf(src.charAt(pos)) == -1)) {
            pos++;
        }
        return pos;
    }

    public static String charsTillString(String needle, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (src.startsWith(needle, pos) == false)) {
            pos++;
        }
        return src.substring(offset, pos);
    }

    public static int skipCharsTillString(String needle, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (src.startsWith(needle, pos) == false)) {
            pos++;
        }
        return pos;
    }


    public static String restOfLine(boolean consumeEol, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (src.startsWith("\r\n", pos) == false && src.charAt(pos) != '\n')) {
            pos++;
        }
        if (consumeEol && pos < len) {
            if (src.startsWith("\r\n", pos)) {
                pos = pos + 2;
            } else if (src.charAt(pos) == '\n') {
                pos++;
            }
        }
        return src.substring(offset, pos);
    }

    public static int skipRestOfLine(boolean consumeEol, String src, int offset) {
        int len = src.length();
        int pos = offset;
        while (pos < len && (src.startsWith("\r\n", pos) == false && src.charAt(pos) != '\n')) {
            pos++;
        }
        if (consumeEol && pos < len) {
            if (src.startsWith("\r\n", pos)) {
                pos = pos + 2;
            } else if (src.charAt(pos) == '\n') {
                pos++;
            }
        }
        return pos;
    }

    public static String exactString(String needle, String src, int offset) {
        int pos = offset;
        if (src.startsWith(needle, offset)) {
            pos += needle.length();
        }
        return src.substring(offset, pos);
    }

}
