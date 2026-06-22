-- Parses the _structure_ of an Org document.
--
-- Notes:
--
-- - Markup (*bold*, _italic_, etc.) is *not* parsed.

def Main =
  block
    Many BlankLine
    $$ = OrgDoc
    Many BlankLine
    Whitespace
    END

def OrgDoc =
  block
    -- header = []
    body = Many (OrgBody false 0)
    let skip = Many BlankLine
    sections = OrgSections 1

def OrgSections l = Many (OrgSectionAt l)

def OrgSectionAt l =
  block
    let skip = Many BlankLine
    let here = GetStream
    case HeadlineMark of
      nothing -> Fail "not a section header"
      just x ->
        if x < l
        then Fail "higher-level section"
        else if x == l
             then
               block
                 let todo = Optional ToDoMark
                 let priority = case todo of
                                  just _ -> Optional PriorityMark
                                  nothing -> nothing
                 let header = SectionTitle
                 -- drawer =
                 Many BlankLine
                 let body = Many (OrgBody false 0)
                 OrgSection x todo priority header body
             else
               block
                 SetStream here
                 OrgSection l nothing nothing [] []

-- A section is marked by one or more asterisks at the start of the line,
-- followed by the section name.  This parses only the asterisks and returns the
-- count of parsed asterisks.
--
-- This parser assumes it is called at the beginning of the line.
--
def HeadlineMark : maybe (uint 8) =
  Optional
    block
      $headline_mark
      $$ = many (l = 1) {$headline_mark; ^ l + 1}
      $whitespace

def ToDoMark =
  block
    $$ = First
           Match "TODO"
           Match "DONE"
    Whitespace1

def PriorityMark =
  block
    Match "[#"
    $$ = First
           $[ "ABC" ]
           block
             let num = many (s = 0) (10 * s + $[ '0' .. '9' ])
             num == 0 is false
             ^ num
    $[ ']' ]
    Whitespace1

def OrgSection l t p h b =
  block
    level = l
    todo = t
    priority = p
    header = h
    body = b
    sections = OrgSections (l + 1)


def SectionTitle =
  block
    -- TODO state
    $$ = Line false -- ok, already consumed the HeadlineMark
    -- cookie ([/] or [%] or expansion thereof)
    -- tags
    First
      END  -- last line, line not even ended
      { EndLine; Accept } -- normal section header ending
      { EndLine; Whitespace; END }  -- last line in file

def OrgBody inDrawer n =
  block
    Many BlankLine
    First
      dashList = DashList inDrawer n
      plusList = PlusList inDrawer n
      enumList = EnumList inDrawer n
      splatList = if n > 0
                  then SplatList inDrawer n
                  else { $whitespace; SplatList inDrawer (n + 1) }
      drawer = Drawer inDrawer n
      aBlock = Block inDrawer n
      setting = Setting
      paragraph = { WhitespaceN n; Paragraph inDrawer }

----------------------------------------------------------------------

-- A list is a number of items where each item is indicted by a list marker.  The
-- list marker can be contained by the list entry, which is a body, and the list
-- entry can be followed by multiple members which are also part of the list body.
-- Rules:
--  1. Three types of lists: unordered, ordered, and description
--  2. List markers, by list type:
--     a. Unordered: '- ', '+ ', '* '
--        1. If the list marker is the splat ('* ') then it cannot appear
--           at the start of the line because that is a section marker.
--     b. Ordered: 'nn. ', 'nn) '
--        1. The nn must be one or more digits.  The exact digits do not matter
--           except for the first: the list items will be numbered in
--           monotonically increasing numeric values.
--     c. Description: same as unordered, but includes a '::' on the first line
--        to distinguish between the term and the corresponding description.
--        1. The '::' marker must appear on the first line of a list item
--        2. The first list entry must have the description marker, otherwise
--           the "term" portion is dropped from all entries.
--        3. Non-first entries that don't have the description marker will
--           not show a term and only the description
--  3. All subsequent content indented greater than the level of the (beginning
--     of the) list marker is part of the body of the list item associated with
--     the list marker.
--     a. This includes sub-lists.
--  4. All list markers at the same indentation as the first are other entries
--     in the same list.
--     a. The exception is the outermost list, which allows list markers at
--        a lesser indentation than the current one, although that lesser
--        indentation becomes the new indentation for that level of the list.
--  5. Two blank lines will also terminate a list (at all levels)
--  6. All list items may start with a checkbox on the item line, containing
--     state:
--     a. "[ ]" - empty checkbox
--     b. "[-]" - partially completed
--     c. "[X]" - completed
--     d. Normally, description lists are not enabled for checkboxes, but this
--        is configurable in Emacs, so here if it exists it will be parsed as
--        if enabled.

def ListMark M n : uint 64 =
  block
    First
      block
        WhitespaceN n
        M
        EndListMark
        ^n
      block
        let here = GetStream
        WhitespaceN n -- at least this amount already present
        SetStream here
        ListMark M (n + 1)

def EndListMark =
  First
    { $whitespace; Accept }
    block
      let here = GetStream
      EndLine
      SetStream here
      Accept
    END

-- n.b. cannot use: call sites trigger a Daedalus "incompatible recursion" error.
-- def ListKind M n =  -- n is minimum indentation level of list marker
--   block
--     Optional BlankLine
--     let lii = ListMark M n
--     SomeSepBy (ListItem lii) { Many (lii .. lii) $whitespace;
--                                M;
--                                $whitespace
--                              }

def ListItem inDrawer lii =
  block
    checkbox = Optional CheckBox
    term = Optional ListTerm
    entry = MainListEntry inDrawer (lii + 1)
    more = Many (OrgBody inDrawer (lii + 1))

def CheckBox =
  block
    $['[']
    $$ = $[" -X"]
    $[']']
    $whitespace

def ListTerm =
  First
    block
      $$ = Many {Whitespace; TermWord}
      Whitespace1
      Match "::"
      First
        @$whitespace
        block
          let here = GetStream
          EndLine
          SetStream here
        END
    block
      Match "::"
      First
        @$whitespace
        block
          let here = GetStream
          EndLine
          SetStream here
        END
      ^ []

def $colon = ":"

def TermWord =
  First
    block
      let c1 = $[ $any - ($end_word | $colon) ]
      let c2 = $[ $any - ($end_word | $colon) ]
      let rest = Many $[$any - $end_word]
      build (emitArray (emit (emit builder c1) c2) rest)
    block
      let c1 = $[ $any - $end_word ]
      let here = GetStream
      $end_word
      SetStream here
      ^ [c1]

-- similar to Paragraph, but no special handling of first word, first line,
-- because we already just parsed the list mark
def MainListEntry inDrawer lii =
  block
    let l1 = RemainingLine
    First
      block
        EOP inDrawer
        ^[ l1 ]
      block
        let ls = Many {EndLine; WhitespaceN lii; Line inDrawer}
        EOP inDrawer
        build (emitArray (emit builder l1) ls)
      block
        EndLine
        ^[ l1 ]

def DashListMark n = ListMark $["-"] n

-- def DashList n = ListKind $["-"] n
def DashList inDrawer n =
  block
    let lii = DashListMark n
    SomeSepBy (ListItem inDrawer lii)
              { WhitespaceN lii;
                $["-"];
                EndListMark
              }

def PlusListMark n = ListMark $["+"] n

-- def PlusList n = ListKind $["+"] n
def PlusList inDrawer n = -- n is minimum indentation level of list marker
  block
    let lii = PlusListMark n
    SomeSepBy (ListItem inDrawer lii)
              { WhitespaceN lii;
                $["+"];
                EndListMark
              }

def SplatListMark n = ListMark $["*"] n

-- def SplatList n = ListKind $["*"] n
def SplatList inDrawer n = -- n is minimum indentation level of list marker
  block
    let lii = SplatListMark n
    SomeSepBy (ListItem inDrawer lii)
              { WhitespaceN lii;
                $["*"];
                EndListMark
              }


def EnumMarker = { Many (1..) $[ "0123456789" ]; $[ ".)" ] }

def EnumListMark n = ListMark EnumMarker n

-- def EnumList n = ListKind EnumMarker n
def EnumList inDrawer n = -- n is minimum indentation level of list marker
  block
    let lii = EnumListMark n
    SomeSepBy (ListItem inDrawer lii)
              { WhitespaceN lii;
                EnumMarker;
                EndListMark
              }

----------------------------------------------------------------------

def Drawer inDrawer n =
  block
    inDrawer is false
    WhitespaceNMore n
    name = DrawerName
    contents = Many (OrgBody true n)
    WhitespaceNMore n
    EndDrawer

def DrawerName =
  block
    $colon
    $$ = Many (1..) $[ ( 'a' .. 'z' | 'A' .. 'Z' | "0123456789-_") ]
    $colon
    -- $$ != "end"
    -- $$ != "End"
    -- $$ != "END"
    Whitespace
    EndLine

def EndDrawer =
  block
    Many BlankLine
    Whitespace
    AnyCaseWord ":end:"
    Whitespace
    First
      @EndLine
      END

----------------------------------------------------------------------

-- A block is started by #+BEGIN_type, and continues until a corresponding
-- #+END_type.  The beginning mark should be indented as any other element to
-- specify where it belongs, but the contents of the block and the end tag are
-- verbatim and indentation is relative to column 0.
--
-- The end tag may have nothing else on the same line, but the start tag may have
-- various arguments.

def Block inDrawer n =
  block
    WhitespaceNMore n
    type = BlockType
    args = RemainingLine
    contents = Many {EndLine; BlockLine type}
    EndLine
    EndBlock type

def BlockType = { AnyCaseWord "#+BEGIN_"; Word }

def BlockLine type =
  block
    let eb = Optional (EndBlock type)
    case eb of
      just e -> Fail "end of block" -- reverts parse of EndBlock
      nothing -> Many $[ $any - "\n" ]

def EndBlock type =
  block
    Whitespace
    AnyCaseWord "#+END_"
    Match type
    Whitespace
    First
      @EndLine
      END


----------------------------------------------------------------------

-- A paragraph is at least one word, and possibly several lines of words,
-- terminated by a blank line, a section, or the end of the input.
--
-- In general, we don't care about the level of indentation of any of the lines
-- of a paragraph.
--
-- Returns a list of lines, where each line is a list of words, and each word is
-- a list of uint 8 byte values.
--
-- Note that if we are starting a paragraph without knowing anything, it should
-- not be a section but could be anything else because we've already checked for
-- the other things.  However, after the first line, all subsequent lines should
-- be checked to make sure they are not any kind of other thing (header, drawer,
-- block, list, etc.).

def Paragraph inDrawer : [ [ [uint 8] ] ] =
  block
    let l1 = ParaFirstLine inDrawer
    let ls = Many { EndLine; Line inDrawer }
    EOP inDrawer
    build (emitArray (emit builder l1) ls)

def ParaFirstLine inDrawer : [[uint 8]] =
  First
    block
      let w1 = StartOfParaWord inDrawer
      let ws = Many { Whitespace1; Word }
      build (emitArray (emit builder w1) ws)
    block
      Whitespace1
      let w1 = FirstParaWord inDrawer
      let ws = Many { Whitespace1; Word }
      build (emitArray (emit builder w1) ws)


def EOP inDrawer = -- End of Paragraph
  First
    END  -- last paragraph, line not even ended
    block
      Whitespace
      EndLine
      First
        { BlankLine; Accept } -- normal paragraph ending (blank line)
        { Whitespace; END }  -- last line in file
        block
          let here = GetStream
          First
            @$headline_mark
            @{ Whitespace; $unordered_list_mark }
            @{ Whitespace; EnumMarker }
            @{ Setting; }
            if inDrawer
              then @{ let here = GetStream; EndDrawer; SetStream here }
              else @{ Whitespace; DrawerName }
            @BlockType
          SetStream here
    block
      let here = GetStream
      if inDrawer
        then @{ let here = GetStream; EndDrawer; SetStream here }
        else @{ Whitespace; DrawerName }
      SetStream here

-- A line is a series of one or more words separated by whitespace, and possibly
-- preceded by whitespace.
def Line inDrawer : [[uint 8]] =
  First
    block
      let w1 = StartOfLineWord inDrawer
      let ws = Many { Whitespace1; Word }
      Whitespace
      build (emitArray (emit builder w1) ws)
    block
      Whitespace1
      let w1 = FirstWord inDrawer
      let ws = Many { Whitespace1; Word }
      Whitespace
      build (emitArray (emit builder w1) ws)

def RemainingLine =
  First
    block
      Whitespace
      let w1 = Word
      let ws = Many { Whitespace1; Word }
      build (emitArray (emit builder w1) ws)
    ^ []

-- One or more occurrences of the first parser, separated by the second.
def SomeSepBy P B =
  block
    let f = P
    let rest = Many {B; P}
    build (emitArray (emit builder f) rest)

def $whitespace = " \t"
def $end_word = " \t\n"
def $headline_mark = "*"
def $unordered_list_mark = "*-+"

-- A word is a set of characters up to whitespace.  Any characters can be present
-- in the word.

def Word = Many (1..) $[$any - $end_word]

-- This is already determined to be a Paragraph (e.g. we thought it might be a
-- block but there was no block ending tag), so don't look for paragraph-ending
-- words.
def StartOfParaWord inDrawer =
  First
    block
      let open = Many (1..) $headline_mark
      -- If the line starts with splats, they cannot be followed by whitespace
      -- (or be the only thing on the line) because that is a Header.
      let next = $[ $any - $end_word ]
      let rest = Many $[ $any - $end_word ]
      build (emitArray (emit (emitArray builder open) next) rest)
    block
      -- regular, non-headline first word
      let open = $[ $any - ($end_word | $headline_mark) ]
      let next = $[ $any - ($end_word | $headline_mark) ]
      let rest = Many $[ $any - $end_word ]
      let word = build (emitArray (emit (emit builder open) next) rest)
      CheckSpecialWords true inDrawer word
    block
      -- regular, non-headline, single character first word
      [ $[ $any - ($end_word | $headline_mark) ] ]

-- Same as StartOfParaWord, but for an indented first paragraph word.
def FirstParaWord inDrawer =
  First
    block
      -- regular, non-headline first word
      CheckSpecialWords true inDrawer Word
    block
      -- regular, non-headline, single character first word
      [ $[ $any - $end_word ] ]

-- Called when the word is at the start of the line.  Do not accept a word that
-- could be a headline mark, or a list marker or a drawer or a block.
def StartOfLineWord inDrawer =
  First
    block
      let open = Many (1..) $headline_mark
      -- If the line starts with splats, they cannot be followed by whitespace
      -- (or be the only thing on the line) because that is a Header.
      let next = $any - $end_word
      let rest = Many $[ $any - $end_word ]
      build (emitArray (emit (emitArray builder open) next) rest)
    UnorderedListLikeWord true
    EnumListLikeWord
    block
      -- regular, non-headline first word
      let open = $regular_start_char
      let next = $[ $any - ($end_word | $headline_mark) ]
      let rest = Many $[ $any - $end_word ]
      let word = build (emitArray (emit (emit builder open) next) rest)
      CheckSpecialWords false inDrawer word
    block
      -- regular, non-headline, single character first word
      $$ = [ $regular_start_char ]
      let here = GetStream
      $end_word
      SetStream here

def $regular_start_char = $any - ( $end_word
                                 | $headline_mark
                                 | $unordered_list_mark
                                 )

-- Called for the first word on a line that contains some initial whitespace.
-- Do not accept a word that could be a list marker or a drawer or a block.
-- Essentially StartOfLineWord but without the check for headline_mark.
def FirstWord inDrawer =
  First
    UnorderedListLikeWord false
    EnumListLikeWord
    block
      -- regular, non-headline first word
      let open = $regular_first_char
      let next = $[ $any - ($end_word) ]
      let rest = Many $[ $any - $end_word ]
      let word = build (emitArray (emit (emit builder open) next) rest)
      CheckSpecialWords false inDrawer word
    block
      -- regular, non-headline, single character first word
      $$ = [ $regular_first_char ]
      let here = GetStream
      $end_word
      SetStream here

def $regular_first_char = $any - ( $end_word
                                 | $unordered_list_mark
                                 )

def CheckSpecialWords knownPara inDrawer word = -- see also 'def Setting ='
      case (lowercase word) of
        "#+archive:" -> Fail "bad word: setting"
        "#+author:" -> Fail "bad word: setting"
        "#+category:" -> Fail "bad word: setting"
        "#+columns:" -> Fail "bad word: setting"
        "#+constants:" -> Fail "bad word: setting"
        "#+description:" -> Fail "bad word: setting"
        "#+email:" -> Fail "bad word: setting"
        "#+filetags:" -> Fail "bad word: setting"
        "#+html_doctype:" -> Fail "bad word: setting"
        "#+html_container:" -> Fail "bad word: setting"
        "#+html_link_home:" -> Fail "bad word: setting"
        "#+html_link_up:" -> Fail "bad word: setting"
        "#+html_mathjax:" -> Fail "bad word: setting"
        "#+html_head:" -> Fail "bad word: setting"
        "#+html_head_extra:" -> Fail "bad word: setting"
        "#+keywords:" -> Fail "bad word: setting"
        "#+language:" -> Fail "bad word: setting"
        "#+latex_class:" -> Fail "bad word: setting"
        "#+latex_class_options:" -> Fail "bad word: setting"
        "#+latex_class_pre:" -> Fail "bad word: setting"
        "#+latex_compiler:" -> Fail "bad word: setting"
        "#+latex_header:" -> Fail "bad word: setting"
        "#+latex_header_extra:" -> Fail "bad word: setting"
        "#+link:" -> Fail "bad word: setting"
        "#+name:" -> Fail "bad word: link target"
        "#+priorities:" -> Fail "bad word: setting"
        "#+property:" -> Fail "bad word: setting"
        "#+options:" -> Fail "bad word: setting"
        "#+setupfile:" -> Fail "bad word: setting"
        "#+startup:" -> Fail "bad word: setting"
        "#+subtitle:" -> Fail "bad word: setting"
        "#+tags:" -> Fail "bad word: setting"
        "#+title:" -> Fail "bad word: setting"
        "#+todo:" -> Fail "bad word: setting"
        ":end:" -> if inDrawer then Fail "drawer ending keyword" else ^word
        _ ->
          if ! knownPara && length word >= 8 && Startswith "#+begin_" (lowercase word)
          then Fail "start of a block"
          else if (! knownPara
                   && ! inDrawer
                   && Index word 0 == ':'
                   && Index word (length word - 1) == ':')
               then Fail "drawer start word"
               else ^word

def UnorderedListLikeWord isLineStart =
    block
      let open = $unordered_list_mark
      -- A list mark cannot be followed by whitespace or be the only thing on
      -- the line because that is a list item.
      let next = if isLineStart
                 then $[ $any - ($headline_mark | $end_word) ]
                 else $[ $any - $end_word ]
      let rest = Many $[ $any - $end_word ]
      build (emitArray (emit (emit builder open) next) rest)

def EnumListLikeWord =
    block
      let open = Many (1..) $[ '0' .. '9' ]
      First
        block
          let next = $[".)"]
          let rest = Many (1..) $[ $any - $end_word ]
          build (emitArray (emit (emitArray builder open) next) rest)
        block
          let next = $[ $any - ".)" ]
          let rest = Many $[ $any - $end_word ]
          build (emitArray (emit (emitArray builder open) next) rest)

def DrawerLikeWord inDrawer =
  block
    let open = $colon
    if inDrawer
     then
      First
        block
          AtWordEnd
          ^ [':']
        block
          let word = Word
          case word of
            "end:" -> Fail "drawer ending keyword"
            _ -> build (emitArray (emit builder open) word)
     else
      First
        block
          let rest = Many (1..) $[ $any - ($end_word | $colon) ]
          AtWordEnd
          build (emitArray (emit builder open) rest)
        block
          AtWordEnd
          ^ [':']


----------------------------------------------------------------------

def Setting =
  block
    Whitespace
    Match "#+"
    keyword = First  -- also update CheckSpecialWords
      AnyCaseWord "ARCHIVE"
      AnyCaseWord "AUTHOR"
      AnyCaseWord "CATEGORY"
      AnyCaseWord "COLUMNS"
      AnyCaseWord "CONSTANTS"
      AnyCaseWord "DESCRIPTION"
      AnyCaseWord "EMAIL"
      AnyCaseWord "FILETAGS"
      AnyCaseWord "HTML_DOCTYPE"
      AnyCaseWord "HTML_CONTAINER"
      AnyCaseWord "HTML_LINK_HOME"
      AnyCaseWord "HTML_LINK_UP"
      AnyCaseWord "HTML_MATHJAX"
      AnyCaseWord "HTML_HEAD"
      AnyCaseWord "HTML_HEAD_EXTRA"
      AnyCaseWord "KEYWORDS"
      AnyCaseWord "LANGUAGE"
      AnyCaseWord "LATEX_CLASS_OPTIONS"
      AnyCaseWord "LATEX_CLASS_PRE"
      AnyCaseWord "LATEX_CLASS"
      AnyCaseWord "LATEX_COMPILER"
      AnyCaseWord "LATEX_HEADER"
      AnyCaseWord "LATEX_HEADER_EXTRA"
      AnyCaseWord "NAME"  -- actually a link target, not a setting
      AnyCaseWord "OPTIONS"
      AnyCaseWord "PRIORITIES"
      AnyCaseWord "PROPERTY"
      AnyCaseWord "SETUPFILE"
      AnyCaseWord "STARTUP"
      AnyCaseWord "SUBTITLE"
      AnyCaseWord "TAGS"
      AnyCaseWord "TITLE"
      AnyCaseWord "TODO"
    $colon
    values =
      First
        { Whitespace1; $$ = RemainingLine; Whitespace; @EndLine <| END }
        {EndLine; ^[]}
        {END; ^[]}


----------------------------------------------------------------------

def AnyCaseWord w =
  map (c0 in w)
    AnyCaseChar c0

def AnyCaseChar c =
  block
    let oc = if c >= 'A' && c <= 'Z'
             then c - 'A' + 'a'
             else if c >= 'a' && c <= 'z'
                  then c - 'a' + 'A'
                  else c
    $[c | oc]

def lowercase w =
  build (for (b = builder; c in w)
           emit b (if c >= 'A' && c <= 'Z' then c - 'A' + 'a' else c))

def Startswith p w =
  for (same = true : bool; i in rangeUp (length p))
    same && (Index p i == Index w i)

def AtWordEnd =
  block
    let here = GetStream
    $end_word
    SetStream here

def Lexeme P = { Whitespace ; P }
def Whitespace = Many $whitespace
def Whitespace1 = { $whitespace; Whitespace }
def WhitespaceN n = Many (n .. n) $whitespace
def WhitespaceNMore n = {Many (n .. n) $whitespace; Whitespace}
def BlankLine = { Whitespace ; EndLine }
def EndWord =
  First
    block
      $end_word
      ^ [] : [uint 8]
    block
      END
      ^ []
def EndLine = $["\n"]
