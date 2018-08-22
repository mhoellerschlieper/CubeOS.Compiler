{*******************************************************************************
    TMemoEx v2.3
    A replacement of a standard VCL TMemo component.

    Copyright (c) 2001-2, TMemoEx.com team
    http://www.tmemoex.com, support@tmemoex.com

    Änderungen: Marcus Höller-Schlieper
                EVIDENT Bingen 2003

Neu  : - diverse Properties:
       - Dokumentation
       - Proportionalschrift:
            dadurch entfällt größtenteils FLastVisibleCol, da diese durch die Proportionalschrift
            nicht zu berechnen ist!!!
       - Suche und Popup-Menü

Bugs : - Texte, die zur Designzeit definiert werden liegen teilweise hinter dem
         fixen Text (23.10.2003)

*******************************************************************************}

Unit MemoEx;

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ClipBrd, MemoExScrollBar, Menus, MemoexSearch;

Const
  Blank             = #160;

  WM_EDITCOMMAND    = WM_USER + $101;

  ME_EX_STYLE_DEFAULT = 0;              //  Extended text style

  //  Case conversion operations codes
  ME_CASE_CONVERT_UPPER = 0;            //  Convert to upper case
  ME_CASE_CONVERT_LOWER = 1;            //  Convert to lower case
  ME_CASE_CONVERT_INVERT = 2;           //  Invert case

  // MHS+
  // MenuItem -  Konstanten
  it_None           = 0;
  it_Undo           = 1;
  it_Redo           = 2;
  it_Cut            = 3;
  it_Copy           = 4;
  it_Paste          = 5;
  it_Delete         = 6;
  it_SelectAll      = 7;
  it_Search         = 8;

  SListIndexError   = '';
Type
  TSearchOption = (
    soBackwards,                        // search backwards instead of forward
    soEntireScope,                      // search in entire text
    soIgnoreNonSpacing, // ignore non-spacing characters in search
    soMatchCase,                        // case sensitive search
    soPrompt,                           // ask user for each replace action
    soRegularExpression,                // search using regular expressions
    soReplace,                          // do a replace
    soReplaceAll,                       // replace all occurences
    soSelectedOnly,                     // search in selected text only
    soSpaceCompress, // handle several consecutive white spaces as one white space
    // so "ab   cd" will match "ab cd" and "ab        cd"
    soWholeWord);                       // match whole words only
  TSearchOptions = Set Of TSearchOption;

  TReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TCellRect = Record
    Width: integer;
    Height: integer;
  End;

  TLineAttr = Record
    FC, BC: TColor;
    Style: TFontStyles;
    ex_style: byte;
  End;

  TCustomMemoEx = Class;

  {
    Line of attributes.
    Unlimited array helps to not to think about very long lines.
  }
  TLineAttrs = Array Of TLineAttr;

  {
    Line of selected symbols.
  }
  TSelAttrs = Array Of boolean;

  {
    User-defined attributes provider.
  }
  TOnGetLineAttr = Procedure(Sender: TObject; Const Line: String; Index:
    integer;
    Const SelAttrs: TSelAttrs; Var Attrs: TLineAttrs) Of Object;

  {
    Event which fires when gutter is being painted.
  }
  TOnPaintGutter = Procedure(Sender: TObject; Canvas: TCanvas) Of Object;

  {
    Event which fires on status (insert/overwrite mode, record mode,
    undo buffer, selection) change.
  }
  TOnChangeStatus = TNotifyEvent;

  {
    Event which fires on clipboard change.
    CanPaste is true when there is a text in clipboard.
  }
  TOnChangeClipboardState = Procedure(Sender: TObject; Const CanPaste: boolean)
    Of Object;

  {
    Event which fires when user click on the word.
    The word is the text with one style.
    WordText contains text of the word.
    WordStyle contains style of the word.
  }
  TOnWordClick = Procedure(Sender: TObject; Const WordText: String; WordStyle:
    word) Of Object;

  {
    Event which fires when user move mouse over the text.
    WordStyle contains style of the word under mouse cursor.
    _Cursor contains one of the available cursor images index.
  }
  TOnMouseOver = Procedure(Sender: TObject; WordStyle: word; Var _Cursor:
    TCursor) Of Object;

  {
    Event which fires when the line is being break.
    Original contains not breaked yet line.
    _New contains part of the line which will be wrapped.
  }
  TOnBreakLine = Procedure(Sender: TObject; Const Original: String; Var _New:
    String) Of Object;

  {
    Event which fires when two lines are being pasted together.
    Original contains line to which _New will be pasted.
    _New contains line being pated to Original.
  }
  TOnConcatLine = Procedure(Sender: TObject; Const Original: String; Var _New:
    String) Of Object;

  {
    Event which fires when the text is being inserted with InsertTextAtCurrentPos,
    block insert or pasted from clipboard.
  }
  TOnTextInsert = Procedure(Sender: TObject; Var Text: String) Of Object;

  {
    User-defined case conversion routine.
  }
  TOnCaseConversion = Function(Sender: TObject; Conversion: byte; Const Text:
    String): String Of Object;

  {
    Block insert.
    Text contains a text of the block being inserted.
  }
  TOnInsertBlock = Function(Sender: TObject; Var Text: String): boolean Of
    Object;

  {
    Block save.
    Text contains a text of the block for saving in file.
  }
  TOnSaveBlock = Procedure(Sender: TObject; Const Text: String) Of Object;

  {
    MacroID is the identifier of the macro shortcut.
    The result of the event is the text being inserted at current position.
  }
  TOnInsertMacro = Function(Sender: TObject; MacroID: integer): String Of
    Object;

  {
    User-defined block operation.
    MacroID contains identifier of the block operation.
    Text contains the text of the block to operate.
    The result of the event is the modified Text.
  }
  TOnBlockOperation = Function(Sender: TObject; MacroID: integer; Const Text:
    String): String Of Object;

  {
    User-defined operation with auto-completion text.
    Text contains original text for defined auto-completion.
    The result of the event is the modified Text.
  }
  TOnPreprocessCompletion = Function(Sender: TObject; Const ID, Text: String):
    String Of Object;

  {
    Words for auto-change feature.
  }
  PAutoChangeWord = ^TAutoChangeWord;
  TAutoChangeWord = Record
    OldWord, NewWord: String;
  End;

  {
    Paragraph.
    All lines of the file separated by <CR> or <LF> symbols are paragraphs.
    Paragraph contains wrapped lines.
  }
  TParagraph = Record
    FChanged: boolean;                  //  paragraph was changed
    FPreCount,
      FCount: integer;                  //  number of wrapped lines
    FStrings: Array Of String;          //  wrapped lines
    FAttrs: TLineAttrs;                 //  paragraph attributes
  End;

  {
    List of the paragraphs.
  }
  PParagraphList = ^TParagraphList;
  TParagraphList = Array[0..MaxListSize Div 2] Of TParagraph;

  {
    Editor lines storage.
    Like TStrings but with word-wrap features.
  }
  TMemoExStrings = Class(TStrings)
  Private
    FMemoEx: TCustomMemoEx;
    FList: PParagraphList;
    FParaLinesCount, FCount: integer;
    FCapacity: integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;

    {
      Event which fires after loading the file by LoadFromFile.
    }
    FOnAfterLoad: TNotifyEvent;

    {
      Event which fires before saving lines to file by SaveToFile.
    }
    FOnBeforeSave: TNotifyEvent;

    Procedure FinalizeParagraph(Index: integer);
    Procedure FinalizeParagraphs;
    Procedure Recount(Index: integer);
    Function _GetString(ParaIndex: integer): String;
    Procedure _PutString(ParaIndex: integer; S: String);
    Procedure ReformatParagraph(ParaIndex: integer);
    Procedure Reformat;
    Procedure CheckLength(Const st: String);
    Procedure SetLockText(Const Text: String);
    Procedure Grow;
    Procedure InsertItem(Index: integer; Const S: String);
  Protected
    Procedure Changed; Virtual;
    Procedure Changing; Virtual;
    Function Get(Index: integer): String; Override;
    Function GetCapacity: integer; Override;
    Function GetCount: integer; Override;
    Function GetParaLineCount: integer;
    Function GetParaString(Index: integer): String;
    Function GetParagraph(Index: integer): TParagraph;
    Procedure Put(Index: integer; Const S: String); Override;
    Procedure PutParaString(Index: integer; Const S: String);
    Procedure SetCapacity(NewCapacity: integer); Override;
    Procedure SetUpdateState(Updating: Boolean); Override;
    Procedure SetTextStr(Const Value: String); Override;
    Procedure SetInternal(Index: integer; Const Value: String);
    Procedure SetInternalParaStr(Index: integer; Const Value: String);

    {
      AddParaStr adds wrapped line to paragraph.
    }
    Function AddParaStr(ParaIndex: integer; Const S: String): integer;
    Function InsertAtPos(Index: integer; Const S: String): integer;

    Procedure Index2ParaIndex(Index: integer; Var Para, ParaIndex: integer);
    Function GetParagraphByIndex(Index: integer; Var ParaIndex, IndexOffs:
      integer): String;
    Procedure Caret2Paragraph(X, Y: integer; Var ParaIndex, IndexOffs: integer);
    Procedure Paragraph2Caret(ParaIndex, IndexOffs: integer; Var X, Y: integer);
    Function GetParaOffs(ParaIndex: integer): integer;
    Procedure ReLine;

    Procedure SetParaChanged(ParaIndex: integer);

    Property Internal[Index: integer]: String Write SetInternal;
    Property InternalParaStrings[Index: integer]: String Write
    SetInternalParaStr;
    Property Paragraphs[Index: integer]: TParagraph Read GetParagraph;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Clear; Override;

    Procedure BeginUpdate;
    Procedure EndUpdate;

    {
      Add, Delete and Insert used for work with paragraphs.
      It was done for compatibility with TStrings.
      When adding the line (paragraph) it will be automatically wrapped.
    }
    Function Add(Const S: String): integer; Override;
    Procedure Delete(Index: integer); Override;
    Procedure Insert(Index: integer; Const S: String); Override;

    Procedure LoadFromFile(Const FileName: String); Override;
    Procedure SaveToFile(Const FileName: String); Override;

    {
      ParaLineCount contains number of all wrapped lines in list.
      Count contains number of paragraphs in list.
    }
    Property ParaLineCount: integer Read GetParaLineCount;
    {
      ParaStrings return wrapped line.
      Strings return paragraph line.
    }
    Property ParaStrings[Index: integer]: String Read GetParaString Write
    PutParaString;

    Property OnChange: TNotifyEvent Read FOnChange Write FOnChange;
    Property OnChanging: TNotifyEvent Read FOnChanging Write FOnChanging;
  End;

  {
    Bookmark.
  }
  TBookmark = Record
    X, Y: integer;                      //  coordinates
    Valid: boolean;                     //  is bookmark valid
  End;

  TBookmarkNum = 0..9;
  TBookmarks = Array[TBookmarkNum] Of TBookmark;

  TEditorClient = Class
  Private
    FMemoEx: TCustomMemoEx;
    Top: integer;
    Function Left: integer;
    Function Height: integer;
    Function Width: integer;
    Function ClientWidth: integer;
    Function ClientHeight: integer;
    Function ClientRect: TRect;
    Function BoundsRect: TRect;
    Function GetCanvas: TCanvas;
    Property Canvas: TCanvas Read GetCanvas;
  End;

  TGutter = Class(TObject)
  Private
    FMemoEx: TCustomMemoEx;
    FFont: TFont;
    FDrawBitmap: TBitmap;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Paint;
    Procedure Invalidate;
  End;

  TEditCommand = word;

  TMacro = String;

  TEditKey = Class
  Public
    Key1, Key2: Word;
    Shift1, Shift2: TShiftState;
    Command: TEditCommand;
    Constructor Create(Const ACommand: TEditCommand; Const AKey1: word;
      Const AShift1: TShiftState);
    Constructor Create2(Const ACommand: TEditCommand; Const AKey1: word;
      Const AShift1: TShiftState; Const AKey2: word;
      Const AShift2: TShiftState);
  End;

  TKeyboard = Class
  Private
    List: TList;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(Const ACommand: TEditCommand; Const AKey1: word;
      Const AShift1: TShiftState);
    Procedure Add2(Const ACommand: TEditCommand; Const AKey1: word;
      Const AShift1: TShiftState; Const AKey2: word;
      Const AShift2: TShiftState);
    Procedure Clear;
    Function Command(Const AKey: word; Const AShift: TShiftState): TEditCommand;
    Function Command2(Const AKey1: word; Const AShift1: TShiftState;
      Const AKey2: word; Const AShift2: TShiftState): TEditCommand;
    Procedure SetDefLayout;
  End;

  EMemoExError = Class(Exception);

  TUndoBuffer = Class;

  TUndo = Class
  Private
    FMemoEx: TCustomMemoEx;
    Function UndoBuffer: TUndoBuffer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx);
    Procedure Undo; Dynamic; Abstract;
    Procedure Redo; Dynamic; Abstract;
  End;

  TUndoBuffer = Class(TList)
  Private
    FMemoEx: TCustomMemoEx;
    FPtr: integer;
    FCancelUndo, InUndo: boolean;
    Function LastUndo: TUndo;
    Function IsNewGroup(Const AUndo: TUndo): boolean;
  Public
    Constructor Create;
    Procedure Add(Var AUndo: TUndo);
    Procedure Undo;
    Procedure Redo;
    Procedure Clear; Override;
    Procedure Delete;
  End;

  TCompletion = Class;
  TOnCompletion = Procedure(Sender: TObject; Var Cancel: boolean) Of Object;

{$IFDEF VER120}
  TWMContextMenu = Packed Record
    Msg: Cardinal;
    hWnd: HWND;
    Case Integer Of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  End;
{$ENDIF}

  TTabStop = (tsTabStop, tsAutoIndent);

  EInvalidRightMarginValue = Class(Exception);

  { TCustomMemoEx }

  TCustomMemoEx = Class(TCustomControl)
  Private
    { internal objects }
    FLines: TMemoExStrings;
    scbHorz: TMemoExScrollBar;
    scbVert: TMemoExScrollBar;
    FMenu: TPopupMenu;
    EditorClient: TEditorClient;
    FGutter: TGutter;
    FKeyboard: TKeyboard;
    FBookmarks: TBookmarks;
    FUpdateLock: integer;
    FUndoBuffer: TUndoBuffer;
    FGroupUndo: boolean;
    FCompletion: TCompletion;

    FCols, FRows: integer;
    FLeftCol, FTopRow: integer;
    FLastVisibleCol, FLastVisibleRow: integer;
    FCaretX, FCaretY: integer;
    FVisibleColCount: integer;
    FVisibleRowCount: integer;

    FFixedText: String;                 //MHS+
    FFixedTextWidth: integer;
    FFixedTextFontColor: TColor;        //MHS+
    FLinesOK: integer;                  //MHS+
    FLinesOKFontColor: TColor;          //MHS+
    FFixedTextLength: Integer;          //MHS+

    FAllRepaint: boolean;
    FCellRect: TCellRect;
    IgnoreKeyPress: boolean;
    WaitSecondKey: Boolean;
    Key1: Word;
    Shift1: TShiftState;

    { internal - selection attributes }
    FSelected: boolean;
    FSelBlock: boolean;                 //  reserved
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
    FUpdateSelBegX, FUpdateSelEndX, FUpdateSelBegY, FUpdateSelEndY: integer;
    FSelStartX, FSelStartY: integer;
    FclSelectBC, FclSelectFC: TColor;

    LastXCoord: Integer;

    { mouse support }
    timerScroll: TTimer;
    MouseMoveX, MouseMoveY, MouseMoveXX, MouseMoveYY: integer;

    { internal }
    FTabPos: Array Of boolean;
    FTabStops: String{$IFDEF VER140}deprecated{$ENDIF};
    FCharWidth: Array Of integer;
    FTabSize, FIndentSize, FAutoIndentSize: integer;
    FSmartIndent, FSmartTab: boolean;

    { internal - primary for TIReader support }
    FEditBuffer: String;
    FPEditBuffer: PChar;
    FEditBufferSize: integer;

    FCompound: integer;
    { FMacro - buffer of TEditCommand, each command represents by two chars }
    FMacro: TMacro;
    FDefMacro: TMacro;

    { visual attributes - properties }
    FBorderStyle: TBorderStyle;
    FGutterColor: TColor;
    FGutterWidth: integer;
    FRightMarginVisible: boolean;
    FRightMargin, FRealRightMargin, FRightMarginMM: integer;
    FRightMarginColor: TColor;
    FScrollBars: TScrollStyle;
    FDoubleClickLine: boolean;
    FBackspaceUnindents: Boolean;
    FAutoIndent: Boolean;
    FKeepTrailingBlanks: Boolean;
    FCursorBeyondEOF: Boolean;
    FCursorBeyondEOL: Boolean;
    FInclusive: Boolean; //  include last symbol into selection or not
    FsearchText: String;

    FSimpleBeginLine: boolean;

    { non-visual attributes - properties }
    FInsertMode: boolean;
    FReadOnly: boolean;
    FModified: boolean;
    FRecording: boolean;

    { Events }
    FOnGetLineAttr: TOnGetLineAttr;
    FOnChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnChangeStatus: TOnChangeStatus;
    FOnChangeClipboardState: TOnChangeClipboardState;
    FOnScroll: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaintGutter: TOnPaintGutter;
    FOnWordClick: TOnWordClick;
    FOnMouseOver: TOnMouseOver;
    FOnBreakLine: TOnBreakLine;
    FOnConcatLine: TOnConcatLine;
    FOnTextInsert: TOnTextInsert;
    FOnCaseConversion: TOnCaseConversion;
    FOnInsertBlock: TOnInsertBlock;
    FOnSaveBlock: TOnSaveBlock;
    FOnInsertMacro: TOnInsertMacro;
    FOnBlockOperation: TOnBlockOperation;

    FOnCompletionIdentifier: TOnCompletion;
    FOnCompletionTemplate: TOnCompletion;
    FOnCompletionDrawItem: TDrawItemEvent;
    FOnCompletionMeasureItem: TMeasureItemEvent;
    FOnPreprocessCompletion: TOnPreprocessCompletion;

    FDrawBitmap: TBitmap;

    FFont: TFont;

    FWantTabs: boolean;

    FWordWrap: boolean;

    FStripInvisible: boolean;

    FParaX, FParaY: integer;

    NextClipViewer: THandle;
    scbVertWidth, scbHorzHeight: integer;
    Max_X: integer;

    mouse_down, mouse_dragged, double_clicked, gutter_clicked: boolean;
    FWordUnderCursor: String;
    FWordStyleUnderCursor: byte;

    bPaintLineNumbers: Boolean;         // Zeilennummern auf Gutter zeichnen

    Procedure SetFont(Value: TFont);
    Procedure SetLinesOKFontColor(Value: TColor);
    Procedure SetLinesOK(Value: Integer);
    Procedure SetFixedTextFontColor(Value: TColor);
    Procedure SetFixedText(Value: String);

    Function Coord2Col(S: String; Coord: Integer): Integer;
    Function Col2Coord(S: String; Col: Integer): Integer;
    Function LineWidth(S: String): Integer;

    Procedure Search;                   // Formular aufrufen
    Procedure DoSearch;                 // Suche ausführen

    Procedure SetMax_X(Const Value: integer);
    Procedure UpdateEditorSize(Const FullUpdate: boolean = true; Const
      RepaintGutter: boolean = true);
    Procedure RedrawFrom(YFrom: integer);
    Function RepaintParagraph(LineIndex: integer): integer;

    Procedure DoCompletionIdentifier(Var Cancel: boolean);
    Procedure DoCompletionTemplate(Var Cancel: boolean);
    Function DoPreprocessCompletion(Const ID, OldText: String): String;

    Procedure ScrollTimer(Sender: TObject);

    Procedure ReLine;
    Function GetDefTabStop(Const X: integer; Const Next: Boolean): integer;
    Function GetTabStop(Const X, Y: integer; Const What: TTabStop;
      Const Next: Boolean): integer;
    Function GetBackStop(Const X, Y: integer): integer;

    Procedure TextAllChangedInternal(Const Unselect: Boolean);

    { property }
    Procedure SetGutterWidth(AWidth: integer);
    Procedure SetGutterColor(AColor: TColor);
    Function GetLines: TStrings;
    Procedure SetBorderStyle(Value: TBorderStyle);
    Procedure SetLines(ALines: TStrings);
    Function GetRealOffs(DefOffs, Index: integer): integer;
    Function GetSelStart: integer;
    Procedure SetSelStart(Const ASelStart: integer);
    Procedure SetSelLength(Const ASelLength: integer);
    Function GetSelLength: integer;
    Procedure SetMode(index: integer; Value: boolean);
    Procedure SetCaretPosition(Const index, Pos: integer);
    Procedure SetCols(ACols: integer);
    Procedure SetRows(ARows: integer);
    Procedure SetScrollBars(Value: TScrollStyle);
    Procedure SetRightMarginVisible(Value: boolean);
    Procedure SetRightMargin(Value: integer);
    Procedure SetRightMarginColor(Value: TColor);

    Function ExtractStringWithStyle(XX, YY: integer; Const From: String; Style:
      word; Const LineAttrs: TLineAttrs): String;
    Procedure GetWordUnderCursor(X, Y: integer);

    Function GetAfterLoad: TNotifyEvent;
    Procedure SetAfterLoad(Value: TNotifyEvent);
    Function GetBeforeSave: TNotifyEvent;
    Procedure SetBeforeSave(Value: TNotifyEvent);

    Procedure SetWordWrap(Value: boolean);
    Procedure SetStripInvisible(Value: boolean);
    Procedure SetSelectedText(Value: boolean);

    Procedure FontChanged(Sender: TObject);

    Function GetPlainText: String;
    Procedure SetPlainText(Const AValue: String);

    Function GetTextForPrinter: String;

    Function GetCaretPos: TPoint;

    Function GetCanUndo: boolean;
    Function GetCanPaste: boolean;

    Procedure CMEnabledChanged(Var Message: TMessage); Message
      CM_ENABLEDCHANGED;
    Procedure OnMenuClick(Sender: TObject);
    Procedure OnMenuPopup(Sender: TObject);
    Function SearchReplace(Const SearchText, ReplaceText: String; Options:
      TSearchOptions): Integer;
    procedure SetTopRow(const Value: integer);

  Protected
    SelAttrs_Size: integer;
    SelAttrs: TSelAttrs;

    Property FSelectedText: boolean Read FSelected Write SetSelectedText;  // MHS++

    Procedure Resize; Override;
    Procedure CreateWnd; Override;
    Procedure CreateParams(Var Params: TCreateParams); Override;
    Procedure Loaded; Override;
    Procedure Paint; Override;
    Procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; Var
      ScrollPos: integer);
    Procedure Scroll(Const Vert: boolean; Const ScrollPos: integer);
    Procedure PaintLine(Const Line: integer; ColBeg, ColEnd: integer);
    Procedure KeyDown(Var Key: Word; Shift: TShiftState); Override;

    Procedure KeyPress(Var Key: Char); Override;
    Procedure InsertChar(Const Key: Char);

    Procedure SetSel(Const ASelX, ASelY: integer);
    Function GetAttrDelta(StartFrom, EndTo: integer; Const LineAttrs:
      TLineAttrs): integer;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer);
      Override;
    Procedure MouseMove(Shift: TShiftState; X, Y: integer); Override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      Override;
    Procedure DblClick; Override;
    Function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos:
      TPoint): boolean; Override;
    Procedure PaintSelection;
    Procedure SetUnSelected;
    Procedure Mouse2Cell(Const X, Y: integer; Var CX, CY: integer);
    Procedure Mouse2Caret(Const X, Y: integer; Var CX, CY: integer);
    Procedure CaretCoord(Const X, Y: integer; Var CX, CY: integer);
    Function PosFromMouse(Const X, Y: integer): integer;
    Procedure SetLockText(Const Text: String);
    Function ExpandTabs(Const S: String): String;

    Procedure CantUndo{$IFDEF VER140};
    deprecated{$ENDIF};

    Procedure SetCaretInternal(X, Y: integer);
    Procedure ValidateEditBuffer;

    Procedure ChangeBookmark(Const Bookmark: TBookmarkNum; Const Valid:
      boolean);
    Procedure InsertText(Const Text: String);
    Procedure BeginRecord;
    Procedure EndRecord(Var AMacro: TMacro);
    Procedure PlayMacro(Const AMacro: TMacro);
    Function YinBounds(AY: integer): boolean;
    Function DoChangeCase(Const st: String; Conversion: byte): String;

    { triggers for descendants }
    Procedure Changed; Dynamic;
    Procedure TextAllChanged; Dynamic;
    Procedure StatusChanged; Dynamic;
    Procedure SelectionChanged; Dynamic;
    Procedure ClipboardChanged; Dynamic;
    Procedure GetLineAttr(Line, LineIdx, LineOffs, LineLen, ColBeg, ColEnd:
      integer; Const ALine: String); Virtual;
    Procedure GetAttr(ALine: String; Line, ColBeg, ColEnd: integer); Virtual;
    Procedure ChangeAttr(Line, ColBeg, ColEnd: integer); Virtual;
    Procedure GutterPaint(Canvas: TCanvas); Dynamic;
    Procedure CompletionIdentifier(Var Cancel: boolean); Dynamic;
    Procedure CompletionTemplate(Var Cancel: boolean); Dynamic;
    Procedure BookmarkCnanged(Bookmark: integer); Dynamic;
    Function GetBookmark(AIndex: integer): TBookmark;
    Procedure SetBookmark(AIndex: integer; ABookmark: TBookmark);

    Property Gutter: TGutter Read FGutter;
    Property Cols: integer Read FCols Write SetCols;
    Property Rows: integer Read FRows Write SetRows;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure Invalidate; Override;
    Procedure Invalidate2;
    Procedure InvalidateGutter;
    Procedure InvalidateLine(Index: integer);

    Procedure WndProc(Var Message: TMessage); Override;

    Procedure SetLeftTop(ALeftCol, ATopRow: integer);

    Function CalcCellRect(Const X, Y: integer): TRect;
    Procedure SetCaret(X, Y: integer);
    Procedure CaretFromPos(Const Pos: integer; Var X, Y: integer);
    Function PosFromCaret(Const X, Y: integer): integer;
    Procedure PaintCaret(Const bShow: boolean);
    Function GetTextLen: integer;
    Function GetSelText: String;
    Procedure SetSelText(Const AValue: String);
    Function GetWordOnCaret: String;
    Procedure BeginUpdate;
    Procedure EndUpdate;
    Procedure MakeRowVisible(ARow: integer);

    Procedure Command(ACommand: TEditCommand); Virtual;
    Procedure PostCommand(ACommand: TEditCommand);
    Procedure InsertTextAtCurrentPos(Const AText: String);
    Procedure ReplaceWord(Const NewString: String);
    Procedure InvalidateBookmarks;
    Procedure BeginCompound;
    Procedure EndCompound;

    Function GetText(Position: longint; Buffer: PChar; Count: longint): longint;

    Procedure ClipBoardCopy{$IFDEF VER140};
    deprecated{$ENDIF};
    Procedure ClipBoardPaste{$IFDEF VER140};
    deprecated{$ENDIF};
    Procedure ClipBoardCut{$IFDEF VER140};
    deprecated{$ENDIF};
    Procedure DeleteSelected{$IFDEF VER140};
    deprecated{$ENDIF};
    Function IsUndoEmpty: boolean{$IFDEF VER140};
    deprecated{$ENDIF};

    Procedure Clear;
    Procedure SetCaretPos(Const AValue: TPoint);
    Procedure ClearSelection;
    Procedure ClearUndo;
    Procedure CopyToClipboard;
    Procedure CutToClipboard;
    Procedure PasteFromClipboard;
    Procedure SelectAll;
    Procedure Undo;

    Procedure MouseWheelScroll(Delta: integer);

    Property LeftCol: integer Read FLeftCol;
    Property TopRow: integer Read FTopRow Write SetTopRow;
    Property VisibleColCount: integer Read FVisibleColCount;
    Property VisibleRowCount: integer Read FVisibleRowCount;
    Property LastVisibleCol: integer Read FLastVisibleCol;
    Property LastVisibleRow: integer Read FLastVisibleRow;
    Property CaretX: integer Index 0 Read FCaretX Write SetCaretPosition;
    Property CaretY: integer Index 1 Read FCaretY Write SetCaretPosition;
    Property CaretPos: TPoint Read GetCaretPos;
    Property Modified: boolean Read FModified Write FModified;
    Property SelStart: integer Read GetSelStart Write SetSelStart;
    Property SelLength: integer Read GetSelLength Write SetSelLength;
    Property SelText: String Read GetSelText Write SetSelText;
    Property SelectedText: boolean Read FSelected;
    Property PlainText: String Read GetPlainText Write SetPlainText;
    Property Text: String Read GetPlainText Write SetPlainText;
    Property TextForPrinter: String Read GetTextForPrinter;

    Property PaintLineNumbers: Boolean
      Read bPaintLineNumbers Write bPaintLineNumbers;

    Property Bookmarks[Index: integer]: TBookmark Read GetBookmark Write
    SetBookmark;
    Property Keyboard: TKeyboard Read FKeyboard;
    Property CellRect: TCellRect Read FCellRect;
    Property UndoBuffer: TUndoBuffer Read FUndoBuffer;
    Property Recording: boolean Read FRecording;
    Property WordUnderCursor: String Read FWordUnderCursor;
    Property WordStyleUnderCursor: byte Read FWordStyleUnderCursor;
    Property CanUndo: boolean Read GetCanUndo;
    Property CanPaste: boolean Read GetCanPaste;

  Public                                { published in descendants }
    Property Font: TFont Read FFont Write SetFont;

    Property FixedText: String Read FFixedText Write SetFixedText; //MHS+
    Property FixedTextFontColor: TColor Read FFixedTextFontColor Write
      SetFixedTextFontColor;            //MHS+
    Property LinesOK: integer Read FLinesOK Write SetLinesOK; //MHS+
    Property LinesOKFontColor: TColor Read FLinesOKFontColor Write
      SetLinesOKFontColor;              //MHS+

    Property BorderStyle: TBorderStyle Read FBorderStyle Write SetBorderStyle
      Default bsSingle;
    Property Lines: TStrings Read GetLines Write SetLines;
    Property LinesEx: TMemoExStrings Read FLines Write FLines;
    Property ScrollBars: TScrollStyle Read FScrollBars Write SetScrollBars
      Default ssNone;
    Property Cursor Default crIBeam;
    Property Color Default clWindow;

    Property GutterWidth: integer Read FGutterWidth Write SetGutterWidth;
    Property GutterColor: TColor Read FGutterColor Write SetGutterColor Default
      clBtnFace;

    Property RightMarginVisible: boolean Read FRightMarginVisible Write
      SetRightMarginVisible Default true;
    Property RightMargin: integer Read FRightMargin Write SetRightMargin Default
      600;

    Property RightMarginColor: TColor Read FRightMarginColor Write
      SetRightMarginColor Default clBtnFace;
    Property InsertMode: boolean Index 0 Read FInsertMode Write SetMode Default
      true;
    Property ReadOnly: boolean Index 1 Read FReadOnly Write SetMode Default
      false;
    Property DoubleClickLine: boolean Read FDoubleClickLine Write
      FDoubleClickLine Default false;
    Property Completion: TCompletion Read FCompletion Write FCompletion;
    Property TabStops: String Read FTabStops Write FTabStops;
    Property TabSize: integer Read FTabSize Write FTabSize;
    Property IndentSize: integer Read FIndentSize Write FIndentSize;
    Property AutoIndentSize: integer Read FAutoIndentSize Write FAutoIndentSize;
    Property SmartTab: boolean Read FSmartTab Write FSmartTab Default true;
    Property SmartAutoIndent: boolean Read FSmartIndent Write FSmartIndent
      Default true;
    Property BackspaceUnindents: Boolean Read FBackspaceUnindents Write
      FBackspaceUnindents Default true;
    Property AutoIndent: Boolean Read FAutoIndent Write FAutoIndent Default
      true;
    Property KeepTrailingBlanks: Boolean Read FKeepTrailingBlanks Write
      FKeepTrailingBlanks Default false;
    Property CursorBeyondEOF: Boolean Read FCursorBeyondEOF Write
      FCursorBeyondEOF Default false;
    //property CursorBeyondEOL: Boolean read FCursorBeyondEOL write FCursorBeyondEOL default False;
    Property SelForeColor: TColor Read FclSelectFC Write FclSelectFC;
    Property SelBackColor: TColor Read FclSelectBC Write FclSelectBC;

    Property StripInvisible: boolean Read FStripInvisible Write SetStripInvisible
      Default false;
    Property WantTabs: boolean Read FWantTabs Write FWantTabs Default true;
    Property WordWrap: boolean Read FWordWrap Write SetWordWrap Default true;

    Property SimpleBeginLine: boolean Read FSimpleBeginLine Write
      FSimpleBeginLine Default true;

    Property OnAfterLoad: TNotifyEvent Read GetAfterLoad Write SetAfterLoad;
    Property OnBeforeSave: TNotifyEvent Read GetBeforeSave Write SetBeforeSave;

    Property OnGetLineAttr: TOnGetLineAttr Read FOnGetLineAttr Write
      FOnGetLineAttr;
    Property OnChangeStatus: TOnChangeStatus Read FOnChangeStatus Write
      FOnChangeStatus;
    Property OnChangeClipboardState: TOnChangeClipboardState Read
      FOnChangeClipboardState Write FOnChangeClipboardState;
    Property OnScroll: TNotifyEvent Read FOnScroll Write FOnScroll;
    Property OnResize: TNotifyEvent Read FOnResize Write FOnResize;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnChange: TNotifyEvent Read FOnChange Write FOnChange;
    Property OnSelectionChange: TNotifyEvent Read FOnSelectionChange Write
      FOnSelectionChange;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnDblClick: TNotifyEvent Read FOnDblClick Write FOnDblClick;
    Property OnPaintGutter: TOnPaintGutter Read FOnPaintGutter Write
      FOnPaintGutter;
    Property OnMouseOver: TOnMouseOver Read FOnMouseOver Write FOnMouseOver;
    Property OnWordClick: TOnWordClick Read FOnWordClick Write FOnWordClick;
    Property OnBreakLine: TOnBreakLine Read FOnBreakLine Write FOnBreakLine;
    Property OnConcatLine: TOnConcatLine Read FOnConcatLine Write FOnConcatLine;
    Property OnTextInsert: TOnTextInsert Read FOnTextInsert Write FOnTextInsert;
    Property OnCaseConversion: TOnCaseConversion Read FOnCaseConversion Write
      FOnCaseConversion;
    Property OnInsertBlock: TOnInsertBlock Read FOnInsertBlock Write
      FOnInsertBlock;
    Property OnSaveBlock: TOnSaveBlock Read FOnSaveBlock Write FOnSaveBlock;
    Property OnInsertMacro: TOnInsertMacro Read FOnInsertMacro Write
      FOnInsertMacro;
    Property OnBlockOperation: TOnBlockOperation Read FOnBlockOperation Write
      FOnBlockOperation;
    Property OnCompletionIdentifier: TOnCompletion Read FOnCompletionIdentifier
      Write FOnCompletionIdentifier;
    Property OnCompletionTemplate: TOnCompletion Read FOnCompletionTemplate Write
      FOnCompletionTemplate;
    Property OnCompletionDrawItem: TDrawItemEvent Read FOnCompletionDrawItem
      Write FOnCompletionDrawItem;
    Property OnCompletionMeasureItem: TMeasureItemEvent Read
      FOnCompletionMeasureItem Write FOnCompletionMeasureItem;
    Property OnPreprocessCompletion: TOnPreprocessCompletion Read
      FOnPreprocessCompletion Write FOnPreprocessCompletion;
    Property DockManager;
  End;

  TMemoEx = Class(TCustomMemoEx)
  Published
    Property TabOrder;                  // Tabreihenfolge
    Property BorderStyle;               // Ranmen oder nicht Rahmen...
    Property Lines;                     // die einzelnen Zeilen des Memos.
    // zusätzlich zu "Lines.Strings" gibt es
    // noch die Möglichkeit die durch WordWrap
    // erzeugten Zeilen-Darstellung zu bekommen:
    // lines.GetParaString(ZeilenIndex)
    Property ScrollBars;                // Darstellung der ScrollBars
    Property GutterWidth;               // Breite des linken Randes, auf dem
    // gezeichnet werden kann(z.B. Zeilennummern)
    Property GutterColor;               // Farbe des linken Randes
    Property RightMarginVisible;        // zeige den rechten Rand
    Property RightMargin;               // Rechter Rand in Pixel
    Property RightMarginColor;          // LinienFarbe des rechten Randes
    Property InsertMode;                // Einfüge-Modus ja/nein
    Property ReadOnly;                  // nur Lesen ja/nein
    Property DoubleClickLine;           // wenn ja, dann wird ganze Zeile bei
    // Doppelklick ausgewählt
    Property Completion;                // Objekt Vervollständigung
    Property TabStops;                  // ???
    Property TabSize;                   // Anzahl der Chars, die bei Tab weiter
    // gehüpft werden soll
    Property IndentSize; // Anzahl der Blanks, die im ausgewählten Bereich
    // bei Tab hinzugefügt/gelöscht werden
    Property AutoIndent; // Ausrichten des Cursors bei Return ein/aus
    Property AutoIndentSize; // Anzahl der Zeichen die bei "Ausrichten des Cursors bei Return"
    // eingefügt werden
    Property SmartTab;                  // Ausrichtung des Tabs richtet sich nach
    // obiger Zeile
    Property SmartAutoIndent; // Ausrichtung des Cursors nach Return richtet sich nach
    // obiger Zeile
    Property BackspaceUnindents; // Backspace berücksichtigt die Ausrichtungs-Funktionen
    // nicht

    Property KeepTrailingBlanks;        // ????
    Property CursorBeyondEOF;           // Cursor darf hinter End Of File
    //property CursorBeyondEOL;            // Cursor darf hinter End Of Line
    Property SelForeColor;              // Markierungsfarbe
    Property SelBackColor;              // Hindergrundfarbe für Markierungen
    Property Font;                      // Der Zeichensatz

    Property StripInvisible; // versteckte Style-Codes anzeigen/nicht anzeigen

    Property SimpleBeginLine;           // ???

    // ganz neu hinzugekommen:
    Property FixedText;                 // der fixierte Text wird links oben
    // eingeblendet.
    Property FixedTextFontColor;        // Farbe des Textes
    Property LinesOK; // Anzahl der Zeilen die gut sind und durch die Property
    // "GetTextForPrinter" zurückgegeben wird. Alle bösenZeilen
    // werden andersfarbig dargestellt...
    Property LinesOKFontColor;          // ...und zwar in dieser Farbe

    Property OnAfterLoad;
    Property OnBeforeSave;

    Property OnEnter;
    Property OnExit;
    Property OnGetLineAttr;
    Property OnChangeStatus;
    Property OnChangeClipboardState;
    Property OnScroll;
    Property OnResize;
    Property OnKeyDown;
    Property OnKeyPress;
    Property OnKeyUp;
    Property OnChange;
    Property OnSelectionChange;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnDblClick;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;
    Property OnPaintGutter;
    Property OnMouseOver;
    Property OnWordClick;
    Property OnBreakLine;
    Property OnConcatLine;
    Property OnTextInsert;
    Property OnCaseConversion;
    Property OnInsertBlock;
    Property OnSaveBlock;
    Property OnInsertMacro;
    Property OnBlockOperation;
    Property OnCompletionIdentifier;
    Property OnCompletionTemplate;
    Property OnCompletionDrawItem;
    Property OnCompletionMeasureItem;
    Property OnPreprocessCompletion;

    { TCustomControl }
    Property Align;
    Property Enabled;
    Property Color;
    Property Ctl3D;

    Property ParentColor;
    Property ParentFont;
    Property ParentShowHint;
    Property PopupMenu;
    Property ShowHint;
    Property TabStop;
    Property Visible;
    Property Anchors;
    Property AutoSize;
    Property BiDiMode;
    Property Constraints;
    Property UseDockManager Default true;
    Property DockSite;
    Property DragKind;
    Property ParentBiDiMode;

    Property WantTabs Default true;
    Property WordWrap Default true;

    Property OnCanResize;
    Property OnConstrainedResize;
    Property OnDockDrop;
    Property OnDockOver;
    Property OnEndDock;
    Property OnGetSiteInfo;
    Property OnStartDock;
    Property OnUnDock;
  End;

  TCompletionList = (cmIdentifiers, cmTemplates);

  TCompletion = Class(TPersistent)
  Private
    FMemoEx: TCustomMemoEx;
    FPopupList: TListBox;
    FAutoChange: TStrings;
    FAutoChangeList: TList;
    FIdentifiers: TStrings;
    FTemplates: TStrings;
    FItems: TStringList;
    FItemIndex: integer;
    FMode: TCompletionList;
    FDefMode: TCompletionList;
    FItemHeight: integer;
    FTimer: TTimer;
    FEnabled: boolean;
    FVisible: boolean;
    FDropDownCount: integer;
    FDropDownWidth: integer;
    FListBoxStyle: TListBoxStyle;
    FCaretChar: char;
    FCRLF: String;
    FSeparator: String;
    FColoringIdent: Boolean;
    FReducePopup: Boolean;
    FIdentifierColor: TColor;

    Function DoKeyDown(Key: Word; Shift: TShiftState): boolean;
    Procedure DoKeyPress(Key: Char);
    Procedure OnTimer(Sender: TObject);
    Procedure FindSelItem(Var Eq: boolean);
    Procedure ReplaceWord(Const ANewString: String);

    Function Cmp1(Const S1, S2: String): integer;
    Function Cmp2(Const S1, S2: String): boolean;

    Procedure AutoChangeChanged(Sender: TObject);
    Procedure ClearAutoChangeList;
    Procedure UpdateAutoChange;
    Procedure SetStrings(index: integer; AValue: TStrings);
    Function GetItemIndex: integer;
    Procedure SetItemIndex(AValue: integer);
    Function GetInterval: cardinal;
    Procedure SetInterval(AValue: cardinal);
    Procedure MakeItems;
    Function GetItems: TStrings;
  Public
    Constructor Create2(AMemoEx: TCustomMemoEx);
    Destructor Destroy; Override;
    Procedure DropDown(Const AMode: TCompletionList; Const ShowAlways: boolean);
    Procedure DoCompletion(Const AMode: TCompletionList);
    Procedure CloseUp(Const Apply: boolean);
    Procedure SelectItem;
    Property ItemIndex: integer Read GetItemIndex Write SetItemIndex;
    Property Visible: boolean Read FVisible Write FVisible;
    Property Mode: TCompletionList Read FMode Write FMode;
    Property Items: TStringList Read FItems;
  Published
    Property DropDownCount: integer Read FDropDownCount Write FDropDownCount
      Default 6;
    Property DropDownWidth: integer Read FDropDownWidth Write FDropDownWidth
      Default 300;
    Property Enabled: boolean Read FEnabled Write FEnabled Default true;
    Property Separator: String Read FSeparator Write FSeparator;
    Property Identifiers: TStrings Index 0 Read FIdentifiers Write SetStrings;
    Property Templates: TStrings Index 1 Read FTemplates Write SetStrings;
    Property AutoChange: TStrings Index 2 Read FAutoChange Write SetStrings;

    Property ColoringIdent: Boolean Read FColoringIdent Write FColoringIdent;

    Property ItemHeight: integer Read FItemHeight Write FItemHeight;
    Property Interval: cardinal Read GetInterval Write SetInterval;
    Property ListBoxStyle: TListBoxStyle Read FListBoxStyle Write FListBoxStyle;
    Property CaretChar: char Read FCaretChar Write FCaretChar;
    Property CRLF: String Read FCRLF Write FCRLF;
    Property ReducePopup: Boolean Read FReducePopup Write FReducePopup;
    Property IdentifierColor: TColor Read FIdentifierColor Write
      FIdentifierColor;

  End;

Const

  { Editor commands }

  ecCharFirst       = $00;
  ecCharLast        = $FF;
  ecCommandFirst    = $100;
  ecUser            = $8000;            { use this for descendants }

  ecLeft            = ecCommandFirst + 1;
  ecUp              = ecLeft + 1;
  ecRight           = ecLeft + 2;
  ecDown            = ecLeft + 3;
  ecSelLeft         = ecCommandFirst + 9;
  ecSelUp           = ecSelLeft + 1;
  ecSelRight        = ecSelLeft + 2;
  ecSelDown         = ecSelLeft + 3;
  ecPrevWord        = ecSelDown + 1;
  ecNextWord        = ecPrevWord + 1;
  ecSelPrevWord     = ecPrevWord + 2;
  ecSelNextWord     = ecPrevWord + 3;
  ecSelWord         = ecPrevWord + 4;

  ecWindowTop       = ecSelWord + 1;
  ecWindowBottom    = ecWindowTop + 1;
  ecPrevPage        = ecWindowTop + 2;
  ecNextPage        = ecWindowTop + 3;
  ecSelPrevPage     = ecWindowTop + 4;
  ecSelNextPage     = ecWindowTop + 5;

  ecBeginLine       = ecSelNextPage + 1;
  ecEndLine         = ecBeginLine + 1;
  ecBeginDoc        = ecBeginLine + 2;
  ecEndDoc          = ecBeginLine + 3;
  ecSelBeginLine    = ecBeginLine + 4;
  ecSelEndLine      = ecBeginLine + 5;
  ecSelBeginDoc     = ecBeginLine + 6;
  ecSelEndDoc       = ecBeginLine + 7;
  ecSelAll          = ecBeginLine + 8;

  ecScrollLineUp    = ecSelAll + 1;
  ecScrollLineDown  = ecScrollLineUp + 1;

  ecInsertPara      = ecCommandFirst + 101;
  ecBackspace       = ecInsertPara + 1;
  ecDelete          = ecInsertPara + 2;
  ecChangeInsertMode = ecInsertPara + 3;
  ecTab             = ecInsertPara + 4;
  ecBackTab         = ecInsertPara + 5;
  ecIndent          = ecInsertPara + 6;
  ecUnindent        = ecInsertPara + 7;

  ecDeleteSelected  = ecInsertPara + 10;
  ecClipboardCopy   = ecInsertPara + 11;
  ecClipboardCut    = ecClipboardCopy + 1;
  ecClipBoardPaste  = ecClipboardCopy + 2;

  ecDeleteLine      = ecClipBoardPaste + 1;
  ecDeleteWord      = ecDeleteLine + 1;

  ecToUpperCase     = ecDeleteLine + 2;
  ecToLowerCase     = ecToUpperCase + 1;
  ecChangeCase      = ecToUpperCase + 2;

  ecUndo            = ecChangeCase + 1;
  ecRedo            = ecUndo + 1;
  ecBeginCompound   = ecUndo + 2;       { not implemented }
  ecEndCompound     = ecUndo + 3;       { not implemented }

  ecBeginUpdate     = ecUndo + 4;
  ecEndUpdate       = ecUndo + 5;

  ecSetBookmark0    = ecEndUpdate + 1;
  ecSetBookmark1    = ecSetBookmark0 + 1;
  ecSetBookmark2    = ecSetBookmark0 + 2;
  ecSetBookmark3    = ecSetBookmark0 + 3;
  ecSetBookmark4    = ecSetBookmark0 + 4;
  ecSetBookmark5    = ecSetBookmark0 + 5;
  ecSetBookmark6    = ecSetBookmark0 + 6;
  ecSetBookmark7    = ecSetBookmark0 + 7;
  ecSetBookmark8    = ecSetBookmark0 + 8;
  ecSetBookmark9    = ecSetBookmark0 + 9;

  ecGotoBookmark0   = ecSetBookmark9 + 1;
  ecGotoBookmark1   = ecGotoBookmark0 + 1;
  ecGotoBookmark2   = ecGotoBookmark0 + 2;
  ecGotoBookmark3   = ecGotoBookmark0 + 3;
  ecGotoBookmark4   = ecGotoBookmark0 + 4;
  ecGotoBookmark5   = ecGotoBookmark0 + 5;
  ecGotoBookmark6   = ecGotoBookmark0 + 6;
  ecGotoBookmark7   = ecGotoBookmark0 + 7;
  ecGotoBookmark8   = ecGotoBookmark0 + 8;
  ecGotoBookmark9   = ecGotoBookmark0 + 9;

  ecCompletionIdentifiers = ecGotoBookmark9 + 1;
  ecCompletionTemplates = ecCompletionIdentifiers + 1;

  ecRecordMacro     = ecCompletionTemplates + 1;
  ecPlayMacro       = ecRecordMacro + 1;
  ecBeginRecord     = ecRecordMacro + 2;
  ecEndRecord       = ecRecordMacro + 3;

  ecSaveBlock       = ecEndRecord + 1;
  ecInsertBlock     = ecSaveBlock + 1;

  ecInsertMacro0    = ecInsertBlock + 1;
  ecInsertMacro1    = ecInsertMacro0 + 1;
  ecInsertMacro2    = ecInsertMacro0 + 2;
  ecInsertMacro3    = ecInsertMacro0 + 3;
  ecInsertMacro4    = ecInsertMacro0 + 4;
  ecInsertMacro5    = ecInsertMacro0 + 5;
  ecInsertMacro6    = ecInsertMacro0 + 6;
  ecInsertMacro7    = ecInsertMacro0 + 7;
  ecInsertMacro8    = ecInsertMacro0 + 8;
  ecInsertMacro9    = ecInsertMacro0 + 9;
  ecInsertMacroA    = ecInsertMacro0 + 10;
  ecInsertMacroB    = ecInsertMacro0 + 11;
  ecInsertMacroC    = ecInsertMacro0 + 12;
  ecInsertMacroD    = ecInsertMacro0 + 13;
  ecInsertMacroE    = ecInsertMacro0 + 14;
  ecInsertMacroF    = ecInsertMacro0 + 15;
  ecInsertMacroG    = ecInsertMacro0 + 16;
  ecInsertMacroH    = ecInsertMacro0 + 17;
  ecInsertMacroI    = ecInsertMacro0 + 18;
  ecInsertMacroJ    = ecInsertMacro0 + 19;
  ecInsertMacroK    = ecInsertMacro0 + 20;
  ecInsertMacroL    = ecInsertMacro0 + 21;
  ecInsertMacroM    = ecInsertMacro0 + 22;
  ecInsertMacroN    = ecInsertMacro0 + 23;
  ecInsertMacroO    = ecInsertMacro0 + 24;
  ecInsertMacroP    = ecInsertMacro0 + 25;
  ecInsertMacroQ    = ecInsertMacro0 + 26;
  ecInsertMacroR    = ecInsertMacro0 + 27;
  ecInsertMacroS    = ecInsertMacro0 + 28;
  ecInsertMacroT    = ecInsertMacro0 + 29;
  ecInsertMacroU    = ecInsertMacro0 + 30;
  ecInsertMacroV    = ecInsertMacro0 + 31;
  ecInsertMacroW    = ecInsertMacro0 + 32;
  ecInsertMacroX    = ecInsertMacro0 + 33;
  ecInsertMacroY    = ecInsertMacro0 + 34;
  ecInsertMacroZ    = ecInsertMacro0 + 35;

  ecBlockOpA        = ecInsertMacroZ + 1;
  ecBlockOpB        = ecBlockOpA + 1;
  ecBlockOpC        = ecBlockOpA + 2;
  ecBlockOpD        = ecBlockOpA + 3;
  ecBlockOpE        = ecBlockOpA + 4;
  ecBlockOpF        = ecBlockOpA + 5;
  ecBlockOpG        = ecBlockOpA + 6;
  ecBlockOpH        = ecBlockOpA + 7;
  ecBlockOpI        = ecBlockOpA + 8;
  ecBlockOpJ        = ecBlockOpA + 9;
  ecBlockOpK        = ecBlockOpA + 10;
  ecBlockOpL        = ecBlockOpA + 11;
  ecBlockOpM        = ecBlockOpA + 12;
  ecBlockOpN        = ecBlockOpA + 13;
  ecBlockOpO        = ecBlockOpA + 14;
  ecBlockOpP        = ecBlockOpA + 15;
  ecBlockOpQ        = ecBlockOpA + 16;
  ecBlockOpR        = ecBlockOpA + 17;
  ecBlockOpS        = ecBlockOpA + 18;
  ecBlockOpT        = ecBlockOpA + 19;
  ecBlockOpU        = ecBlockOpA + 20;
  ecBlockOpV        = ecBlockOpA + 21;
  ecBlockOpW        = ecBlockOpA + 22;
  ecBlockOpX        = ecBlockOpA + 23;
  ecBlockOpY        = ecBlockOpA + 24;
  ecBlockOpZ        = ecBlockOpA + 25;

  ecBackword        = ecBlockOpZ + 1;
  ecScrollPageUp    = ecBackword + 1;

  ecScrollPageDown  = ecScrollPageUp + 1;
  ecSearch          = ecScrollPageUp + 2;
  ecSearchNext      = ecScrollPageUp + 3;
  ecReplace         = ecScrollPageUp + 4;
  ecReplaceNext     = ecScrollPageUp + 5;

  twoKeyCommand     = High(word);

Const
  __Brackets        = ['(', ')', '[', ']', '{', '}'];
  __StdWordDelims   = [#0..' ', ',', '.', ';', '\', ':', '''', '`']
    { + __Brackets};

Procedure Register;

Implementation

Uses
  Consts, MemoExClipUtils, Math;

Const
  RAEditorCompletionChars = #8 +
    '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmÉÖÓÊÅÍÃØÙÇÕÚÔÛÂÀÏÐÎËÄÆÝß×ÑÌÈÒÜÁÞ¨éöóêåíãøùçõúôûâàïðîëäæýÿ÷ñìèòüáþ¸';
  StIdSymbols       = ['_', '0'..'9', 'A'..'Z', 'a'..'z', 'À'..'ß', 'à'..'ÿ'];
  _StIdSymbols      = ['>', '<', '''', '"', '`', '!', '@', '#', '$', '%', '^',
    '&', '*', '/', '?'] + __Brackets + StIdSymbols + [#127..#255];
  _AutoChangePunctuation = [' ', '`', '~', '!', '@', '#', '$', '%', '^', '&',
    '*', '(', ')', '_', '-', '+', '=', ';', ':', '''', '"', '[', ']', '{', '}',
    ',', '.', '/', '?', '<', '>'];
  Separators        : Set Of char = [#00, ' ', '-', #13, #10, '.', ',', '/',
    '\',
    ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '{', '}'];

Function Spaces(Const N: integer): String;
Var
  i                 : integer;
Begin
  Result := '';
  For i := 1 To N Do
    Result := Result + ' ';
End;

Function HasChar(Const Ch: Char; Const S: String): boolean;
Begin
  Result := Pos(Ch, S) > 0;
End;

Function GetWordOnPos(Const S: String; Const P: integer): String;
Var
  i, Beg            : integer;
Begin
  Result := '';
  If (P > Length(S)) Or (P < 1) Then
    exit;
  For i := P Downto 1 Do
    If S[i] In Separators Then
      break;
  Beg := i + 1;
  For i := P To Length(S) Do
    If S[i] In Separators Then
      break;
  If i > Beg Then
    Result := Copy(S, Beg, i - Beg)
  Else
    Result := S[P];
End;

Function GetWordOnPosEx(Const S: String; Const P: integer; Var iBeg, iEnd:
  integer): String;
Begin
  Result := '';
  If (P > Length(S)) Or (P < 1) Then
    exit;

  iBeg := P;
  If S[P] In Separators Then
    If (P < 2) Or (S[P - 1] In Separators) Then
      inc(iBeg)                         // BUG P < 1 durch P<2 behoben!!!
    Else If Not (S[P - 1] In Separators) Then
      dec(iBeg);

  While (iBeg >= 1) And (iBeg <= length(S)) Do // MHS
    If S[iBeg] In Separators Then
      break
    Else
      dec(iBeg);

  inc(iBeg);
  iEnd := P;
  While iEnd <= Length(S) Do
    If S[iEnd] In Separators Then
      break
    Else
      inc(iEnd);

  If iEnd > iBeg Then
    Result := Copy(S, iBeg, iEnd - iBeg)
  Else
    Result := S[P];
End;

Function SubStr(Const S: String; Const Index: integer; Const Separator: String):
  String;
Var
  i                 : integer;
  pB, pE            : PChar;
Begin
  Result := '';
  If (index < 0) Or ((index = 0) And (Length(S) > 0) And (S[1] = Separator))
    Then
    exit;
  pB := PChar(S);
  For i := 1 To index Do
  Begin
    pB := StrPos(pB, PChar(Separator));
    If pB = Nil Then
      exit;
    pB := pB + Length(Separator);
  End;
  pE := StrPos(pB + 1, PChar(Separator));
  If pE = Nil Then
    pE := PChar(S) + Length(S);
  If Not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) Then
    SetString(Result, pB, pE - pB);
End;

Function ReplaceWordByPhrase(S: String; Const Word, Phrase: String): String;
Var
  LW                : integer;
  P                 : PChar;
  Sm                : integer;
Begin
  LW := Length(Word);
  P := StrPos(PChar(S), PChar(Word));
  While P <> Nil Do
  Begin
    Sm := P - PChar(S);
    S := Copy(S, 1, Sm) + Phrase + Copy(S, Sm + LW + 1, Length(S));
    P := StrPos(PChar(S) + Sm + Length(Phrase), PChar(Word));
  End;
  Result := S;
End;

Function KeyPressed(VK: integer): boolean;
Begin
  Result := GetKeyState(VK) And $8000 = $8000;
End;

Function _CutString(Len: integer; Var S: String): String;
Var
  T                 : String;
  j                 : integer;
Begin
  Result := '';
  If Len >= length(S) Then
    exit;
  T := System.Copy(S, 1, Len);
  j := length(T);
  While j > 1 Do
    If T[j] = #32 Then
      break
    Else
      dec(j);
  If j = 1 Then
    j := Len;
  Result := System.Copy(S, j + 1, length(S));
  S := System.Copy(S, 1, j);
End;

Function _Trim(Const S: String): String;
Var
  I, L              : Integer;
Begin
  L := Length(S);
  I := 1;
  While (I <= L) And (S[I] = ' ') Do
    Inc(I);
  If I > L Then
    Result := ''
  Else
  Begin
    While S[L] = ' ' Do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  End;
End;

Function _TrimLeft(Const S: String): String;
Var
  I, L              : Integer;
Begin
  L := Length(S);
  I := 1;
  While (I <= L) And (S[I] = ' ') Do
    Inc(I);
  Result := Copy(S, I, Maxint);
End;

Function _TrimRight(Const S: String): String;
Var
  I                 : Integer;
Begin
  I := Length(S);
  While (I > 0) And (S[I] = ' ') Do
    Dec(I);
  Result := Copy(S, 1, I);
End;

Type
  TCaretUndo = Class(TUndo)
  Private
    FCaretX, FCaretY: integer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const ACaretX, ACaretY:
      integer);
    Procedure Undo; Override;
    Procedure Redo; Override;
  End;

  TInsertUndo = Class(TCaretUndo)
  Private
    FText: String;
    FOffset: integer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const ACaretX, ACaretY:
      integer;
      Const AText: String);
    Procedure Undo; Override;
  End;

  TReLineUndo = Class(TInsertUndo);

  TInsertTabUndo = Class(TInsertUndo);

  TOverwriteUndo = Class(TCaretUndo)
  Private
    FOldText, FNewText: String;
    FOffset: integer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const ACaretX, ACaretY:
      integer;
      Const AOldText, ANewText: String);
    Procedure Undo; Override;
  End;

  TDeleteUndo = Class(TInsertUndo)
  Public
    Procedure Undo; Override;
  End;

  TDeleteTrailUndo = Class(TDeleteUndo);

  TBackspaceUndo = Class(TDeleteUndo)
  Public
    Procedure Undo; Override;
  End;

  TReplaceUndo = Class(TCaretUndo)
  Private
    FBeg, FEnd: integer;
    FText, FNewText: String;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const ACaretX, ACaretY:
      integer;
      Const ABeg, AEnd: integer; Const AText, ANewText: String);
    Procedure Undo; Override;
  End;

  TDeleteSelectedUndo = Class(TDeleteUndo)
  Private
    FSelBlock: boolean;                 { vertial block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY, FSelOffs: integer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const ACaretX, ACaretY:
      integer;
      Const AText: String; Const ASelBlock: boolean; Const ASelBegX, ASelBegY,
      ASelEndX,
      ASelEndY, ASelOffs: integer);
    Procedure Undo; Override;
  End;

  TSelectUndo = Class(TCaretUndo)
  Private
    FSelBlock: boolean;                 { vertial block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const ACaretX, ACaretY:
      integer;
      Const ASelBlock: boolean; Const ASelBegX, ASelBegY, ASelEndX, ASelEndY:
      integer);
    Procedure Undo; Override;
  End;

  TUnselectUndo = Class(TSelectUndo);

  TBeginCompoundUndo = Class(TUndo)
  Public
    Procedure Undo; Override;
  End;

  TEndCompoundUndo = Class(TBeginCompoundUndo);

  TIndentUndo = Class(TUndo)
  Private
    FIndentY1, FIndentY2, FIndentSize: integer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const AIndentY1, AIndentY2,
      AIndentSize: integer);
    Procedure Undo; Override;
  End;

  TUnindentUndo = Class(TUndo)
  Private
    FIndentY, FIndentSize: integer;
  Public
    Constructor Create(Const AMemoEx: TCustomMemoEx; Const AIndentY,
      AIndentSize: integer);
    Procedure Undo; Override;
  End;

Procedure Err;
Begin
  MessageBeep(0);
End;

Function FindNotBlankCharPos(Const S: String): integer;
Var
  i                 : integer;
Begin
  Result := 1;
  For i := 1 To Length(S) Do
    If S[i] <> ' ' Then
      Exit;
End;

Function ANSIChangeCase(Const S: String): String;
Var
  i                 : integer;
  Up                : ANSIChar;
Begin
  Result := S;
  For i := 1 To Length(Result) Do
  Begin
    Up := ANSIUpperCase(Result[i])[1];
    If Result[i] = Up Then
      Result[i] := ANSILowerCase(Result[i])[1]
    Else
      Result[i] := Up;
  End;
End;

{ TMemoExStrings }

Procedure TMemoExStrings.LoadFromFile(Const FileName: String);
Begin
  BeginUpdate;
  Try
    Inherited LoadFromFile(FileName);
    If Assigned(FOnAfterLoad) Then
      FOnAfterLoad(FMemoEx);
  Finally
    EndUpdate;
  End;
End;

Procedure TMemoExStrings.SaveToFile(Const FileName: String);
Begin
  If Assigned(FOnBeforeSave) Then
    FOnBeforeSave(FMemoEx);
  Inherited SaveToFile(FileName);
End;

{
  Release memory used for paragraphs storage.
}

Procedure TMemoExStrings.FinalizeParagraphs;
Begin
  If FCount <> 0 Then
    Finalize(FList^[0], FCount);
  FCount := 0;
  FParaLinesCount := 0;
End;

{
  Precounting for line quick search.
}

Procedure TMemoExStrings.Recount(Index: integer);
Var
  i                 : integer;
Begin
  For i := Index To FCount - 1 Do
    If i = 0 Then
      FList^[i].FPreCount := 0
    Else
      FList^[i].FPreCount := FList^[i - 1].FPreCount + FList^[i - 1].FCount;
End;

{
  Release memory used for paragraph storage.
}

Procedure TMemoExStrings.FinalizeParagraph(Index: integer);
Begin
  Finalize(FList^[Index]);
End;

{
  Convert line index (Index) to paragraph coordinates (Paragraph:ParagraphWrappedLineIndex).
}

Procedure TMemoExStrings.Index2ParaIndex(Index: integer; Var Para, ParaIndex:
  integer);
Var
  L, H, I           : integer;
Begin
  If (Not FMemoEx.FWordWrap) Or (FParaLinesCount = FCount) Then
  Begin
    Para := Index;
    If Para > FCount - 1 Then
      Para := FCount - 1;
    ParaIndex := 0;
  End
  Else
  Begin
    {
      Paragraph quick search.
    }
    Para := -1;
    ParaIndex := -1;
    L := 0;
    H := FCount - 1;
    While L <= H Do
    Begin
      I := (L + H) Shr 1;
      If Index > FList^[I].FPreCount + FList^[I].FCount - 1 Then
        L := I + 1
      Else
      Begin
        H := I - 1;
        If (Index <= FList^[I].FPreCount + FList^[I].FCount - 1) And
          (Index >= FList^[I].FPreCount) Then
        Begin
          Para := I;
          ParaIndex := Index - FList^[I].FPreCount;
          break;
        End;
      End;
    End;
  End;
End;

{
  Returns paragraph by linear index.
}

Function TMemoExStrings.GetParagraphByIndex(Index: integer; Var ParaIndex,
  IndexOffs: integer): String;
Var
  _P, _PI, i        : integer;
Begin
  IndexOffs := 0;
  ParaIndex := 0;
  Result := '';
  Index2ParaIndex(Index, _P, _PI);
  If (_P = -1) Or (_PI = -1) Then
    Error(SListIndexError, Index);
  ParaIndex := _P;
  For i := 0 To FList^[_P].FCount - 1 Do
  Begin
    Result := Result + FList^[_P].FStrings[i];
    If i < _PI Then
      inc(IndexOffs, length(FList^[_P].FStrings[i]));
  End;
End;

{
  Convert caret coordinates (X:Y) to paragraph coordinates (ParagraphIndex:IndexOffset).
}

Procedure TMemoExStrings.Caret2Paragraph(X, Y: integer; Var ParaIndex,
  IndexOffs: integer);
Var
  _P, _PI, i        : integer;
Begin
  ParaIndex := 0;
  IndexOffs := 0;
  Index2ParaIndex(Y, _P, _PI);
  If (_P = -1) Or (_PI = -1) Then
    Error(SListIndexError, Y);
  ParaIndex := _P;
  IndexOffs := X;
  For i := 0 To _PI - 1 Do
    inc(IndexOffs, length(FList^[_P].FStrings[i]));
End;

{
  Convert paragraph coordinates (ParagraphIndex:IndexOffset) to caret coordinates (X:Y).
}

Procedure TMemoExStrings.Paragraph2Caret(ParaIndex, IndexOffs: integer; Var X,
  Y: integer);
Var
  i, j, k           : integer;
  b                 : boolean;
Begin
  X := 0;
  Y := ParaIndex;
  b := false;
  k := 0;
  For i := 0 To FCount - 1 Do
  Begin
    If i >= Y Then
    Begin
      For j := 0 To FList^[i].FCount - 1 Do
      Begin
        inc(X, length(FList^[i].FStrings[j]));
        If X >= IndexOffs Then
        Begin
          b := true;
          Y := k + j;
          X := IndexOffs - (X - length(FList^[i].FStrings[j]));
          break;
        End;
      End;
      If b Then
        break;
    End;
    inc(k, FList^[i].FCount);
  End;
  If Not b Then
  Begin
    If X > 0 Then
    Begin
      Y := k;
      X := length(FList^[Y].FStrings[FList^[Y].FCount - 1]);
      exit;
    End;
    Y := FCount - 1;
    If Y >= 0 Then
      X := length(FList^[Y].FStrings[FList^[Y].FCount - 1])
    Else
    Begin
      X := 0;
      Y := 0;
    End;
  End;
End;

{
  Returns paragraph offset from text start.
}

Function TMemoExStrings.GetParaOffs(ParaIndex: integer): integer;
Var
  i                 : integer;
Begin
  Result := 0;
  For i := 0 To ParaIndex - 1 Do
    inc(Result, length(Strings[i]) + 2);
End;

{
  Paragraph formatting.
}

Procedure TMemoExStrings.ReformatParagraph(ParaIndex: integer);
Var
  s, t              : String;
  i                 : integer;
Begin
  With FList^[ParaIndex] Do
  Begin
    dec(FParaLinesCount, FCount);
    FChanged := true;
    Finalize(FAttrs);
    FPreCount := 0;
    If FCount > 1 Then
    Begin
      s := Get(ParaIndex);
      FCount := 1;
      FStrings := System.Copy(FStrings, 0, 1);
      FStrings[0] := s;
    End;

    If FMemoEx.FWordWrap Then
    Begin
      i := 0;
      While i <= FCount - 1 Do
      Begin
        //if length(FStrings[i]) > FMemoEx.FRealRightMargin then
        If FMemoEx.LineWidth(FStrings[i]) >= FMemoEx.FRealRightMargin Then  // MHS+
        Begin
          s := FStrings[i];
          //t := _CutString(FMemoEx.FRealRightMargin, s);                   // MHS-
          t := _CutString(FMemoEx.Coord2Col(s, FMemoEx.FRealRightMargin), s);  // MHS+
          FStrings[i] := s;
          inc(FCount);
          SetLength(FStrings, FCount);
          FStrings[FCount - 1] := t;
        End;
        inc(i);
      End;
    End;
    inc(FParaLinesCount, FCount);
  End;
End;

{
  Reformat all text.
}

Procedure TMemoExStrings.Reformat;
Var
  i                 : integer;
Begin
  For i := 0 To FCount - 1 Do
    ReformatParagraph(i);
  Recount(0);
  Changed;
End;

Procedure TMemoExStrings.CheckLength(Const st: String);
Begin
  If length(st) > FMemoEx.Max_X Then
    FMemoEx.SetMax_X(length(st) + 1);
End;

Function TMemoExStrings.GetParaLineCount: integer;
Begin
  Result := FParaLinesCount;
End;

Function TMemoExStrings.GetCount: integer;
Begin
  Result := FCount;
End;

Constructor TMemoExStrings.Create;
Begin
  Inherited Create;
  FParaLinesCount := 0;
  FOnAfterLoad := Nil;
  FOnBeforeSave := Nil;
End;

Destructor TMemoExStrings.Destroy;
Begin
  FOnChange := Nil;
  FOnChanging := Nil;
  Inherited Destroy;
  FinalizeParagraphs;
  SetCapacity(0);
End;

Function TMemoExStrings.InsertAtPos(Index: integer; Const S: String): integer;
Var
  t                 : String;
  i, j              : integer;
Begin
  i := 1;
  t := '';
  Result := Index;
  If S = '' Then
    InsertItem(Result, '')
  Else
  Begin
    j := Result;
    While i <= length(S) Do
    Begin
      If (i = length(S)) Or (S[i] In [#10, #13]) Then
      Begin
        If Not (S[i] In [#10, #13]) Then
          t := t + S[i];
        InsertItem(j, FMemoEx.ExpandTabs(t));
        inc(i);
        inc(j);
        While (i <= length(S)) And (S[i] In [#10, #13]) Do
          inc(i);
        t := '';
        continue;
      End
      Else
        t := t + S[i];
      inc(i);
    End;
  End;
End;

{
  Add paragraph.
}

Function TMemoExStrings.Add(Const S: String): integer;
Begin
  Result := InsertAtPos(FCount, S);
End;

{
  Delete paragraph.
}

Procedure TMemoExStrings.Delete(Index: integer);
Begin
  If (Index < 0) Or (Index >= FCount) Then
    Error(SListIndexError, Index);
  Changing;
  dec(FParaLinesCount, TParagraph(FList^[Index]).FCount);
  Dec(FCount);
  FinalizeParagraph(Index);
  If Index < FCount Then
  Begin
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) *
      SizeOf(TParagraph));
    Recount(Index);
  End;
  Changed;
End;

{
  Insert paragraph.
}

Procedure TMemoExStrings.Insert(Index: integer; Const S: String);
Begin
  If (Index < 0) Or (Index > FCount) Then
    Error(SListIndexError, Index);
  InsertAtPos(Index, S);
End;

Procedure TMemoExStrings.InsertItem(Index: integer; Const S: String);
Begin
  Changing;
  If FCount = FCapacity Then
    Grow;
  If Index < FCount Then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) *
      SizeOf(TParagraph));
  With FList^[Index] Do
  Begin
    pointer(FStrings) := Nil;
    pointer(FAttrs) := Nil;
    FCount := 0;
    FPreCount := 0;
    FChanged := false;
  End;
  Inc(FCount);
  AddParaStr(Index, S);
  Changed;
End;

{
  Add line to paragraph.
}
{$WARNINGS OFF}

Function TMemoExStrings.AddParaStr(ParaIndex: integer; Const S: String):
  integer;
Begin
  If (ParaIndex < 0) Or (ParaIndex >= FCount) Then
    Error(SListIndexError, ParaIndex);
  With FList^[ParaIndex] Do
  Begin
    inc(FCount);
    inc(FParaLinesCount);
    SetLength(FStrings, FCount);
    FStrings[FCount - 1] := FMemoEx.ExpandTabs(S);
    CheckLength(FStrings[FCount - 1]);
  End;
  ReformatParagraph(ParaIndex);
  Recount(ParaIndex);
  Changed;
End;
{$WARNINGS ON}

Procedure TMemoExStrings.Changed;
Begin
  If FMemoEx.FUpdateLock = 0 Then
    FMemoEx.TextAllChanged;
End;

Procedure TMemoExStrings.Changing;
Begin
  //  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
End;

Procedure TMemoExStrings.Clear;
Begin
  If FCount <> 0 Then
  Begin
    Changing;
    FinalizeParagraphs;
    SetCapacity(0);
    Changed;
  End;
End;

Procedure TMemoExStrings.BeginUpdate;
Begin
  inc(FMemoEx.FUpdateLock);
End;

Procedure TMemoExStrings.EndUpdate;
Begin
  dec(FMemoEx.FUpdateLock);
  Changed;
End;

Function TMemoExStrings._GetString(ParaIndex: integer): String;
Var
  i                 : integer;
Begin
  Result := '';

  For i := 0 To FList^[ParaIndex].FCount - 1 Do
  Begin
    Result := Result + FList^[ParaIndex].FStrings[i];
  End;
End;

Function TMemoExStrings.Get(Index: integer): String;
Begin
  If (Index < 0) Or (Index >= FCount) Then
    Result := ''
  Else
    Result := _GetString(Index);
End;

Function TMemoExStrings.GetParagraph(Index: integer): TParagraph;
Begin
  If (Index < 0) Or (Index >= FCount) Then
    Error(SListIndexError, Index);
  Result := FList^[Index];
End;

Function TMemoExStrings.GetParaString(Index: integer): String;
Var
  _P, _PI           : integer;
Begin
  If (Not FMemoEx.FWordWrap) Or (FParaLinesCount = FCount) Then
    Result := Get(Index)
  Else
  Begin
    Index2ParaIndex(Index, _P, _PI);
    If (_P = -1) Or (_PI = -1) Then
      Result := ''                      //  Error(SListIndexError, Index);
    Else
      Result := FList^[_P].FStrings[_PI];
  End;
End;

Function TMemoExStrings.GetCapacity: integer;
Begin
  Result := FCapacity;
End;

Procedure TMemoExStrings.Grow;
Var
  Delta             : integer;
Begin
  If FCapacity > 64 Then
    Delta := FCapacity Div 4
  Else If FCapacity > 8 Then
    Delta := 16
  Else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
End;

Procedure TMemoExStrings._PutString(ParaIndex: integer; S: String);
Var
  old_count, old_precount: integer;
Begin

  old_count := FList^[ParaIndex].FCount;
  old_precount := FList^[ParaIndex].FPreCount;
  dec(FParaLinesCount, TParagraph(FList^[ParaIndex]).FCount);
  FinalizeParagraph(ParaIndex);
  With FList^[ParaIndex] Do
  Begin
    FCount := 1;
    SetLength(FStrings, FCount);
    FStrings[0] := S;
    inc(FParaLinesCount);
  End;
  ReformatParagraph(ParaIndex);
  If old_count <> FList^[ParaIndex].FCount Then
    Recount(ParaIndex)
  Else
    FList^[ParaIndex].FPreCount := old_precount;
End;

Procedure TMemoExStrings.Put(Index: integer; Const S: String);

Begin
  If (Index < 0) Or (Index >= FCount) Then
    Error(SListIndexError, Index);
  CheckLength(S);
  Changing;
  _PutString(Index, S);
  Changed;
End;

Procedure TMemoExStrings.PutParaString(Index: integer; Const S: String);
Var
  _P, _PI           : integer;
  old_count, old_precount: integer;
Begin

  If Not FMemoEx.FWordWrap Then
    Put(Index, S)
  Else
  Begin
    Index2ParaIndex(Index, _P, _PI);
    If (_P = -1) Or (_PI = -1) Then
      Error(SListIndexError, Index);
    Changing;

    old_count := FList^[_P].FCount;
    old_precount := FList^[_P].FPreCount;
    CheckLength(S);
    FList^[_P].FStrings[_PI] := S;
    ReformatParagraph(_P);
    If old_count <> FList^[_P].FCount Then
      Recount(_P)
    Else
      FList^[_P].FPreCount := old_precount;

    Changed;
  End;
End;

Procedure TMemoExStrings.SetCapacity(NewCapacity: integer);
Begin
  ReallocMem(FList, NewCapacity * SizeOf(TParagraph));
  FCapacity := NewCapacity;
End;

Procedure TMemoExStrings.SetUpdateState(Updating: Boolean);
Begin
  If Updating Then
    Changing
  Else
    Changed;
End;

Procedure TMemoExStrings.SetTextStr(Const Value: String);

Var
  Praefix           : String;           // MHS+
  S                 : String;
  Fxlen             : Integer;
Begin
  inc(FMemoEx.FUpdateLock);

  // Anzahl von Blanks voranstellen, damit der fixe Text den Memotext nicht
  // überblendet!!
  // Die Blanks werden natürlich auch ausgedruckt (wichtig und richtig für Formulare)
  If FMemoex.FFixedText = '' Then
  Begin
    S := Value;                         // MHS+
    FMemoex.FFixedTextLength := 0;      // MHS+
    FMemoex.FFixedTextWidth := 0;
  End
  Else
  Begin
    S := TrimLeft(Value);
    If length(s) > 0 Then
      While (s[1] = Blank) Do
        System.Delete(s, 1, 1);

    Praefix := '';
    FMemoex.Canvas.Font := FMemoex.Font;
    FMemoex.Canvas.Font.Style := [fsBold];
    FxLen := FMemoex.Canvas.TextWidth(FMemoex.FFixedText);

    FMemoex.Canvas.Font := FMemoex.Font; // MHS+
    While (FMemoex.Canvas.TextWidth(Praefix) <= FxLen) Do // MHS+
      Praefix := Praefix + Blank;       // MHS+
    // MHS+
    FMemoex.FFixedTextLength := length(Praefix); // MHS+
    FMemoex.FFixedTextWidth := FMemoex.Canvas.Textwidth(Praefix); // MHS+
    S := Praefix + S;                   // MHS+
  End;

  Inherited SetTextStr(FMemoEx.ExpandTabs(S));

  dec(FMemoEx.FUpdateLock);

  If FMemoEx.FUpdateLock = 0 Then
  Begin
    FMemoEx.ClearUndo;
    FMemoEx.TextAllChanged;
    FMemoEx.FCaretX := -1;
    FMemoEx.FCaretY := -1;
    FMemoEx.SetCaretInternal(0, 0);
  End;
End;

Procedure TMemoExStrings.SetParaChanged(ParaIndex: integer);
Begin
  FList^[ParaIndex].FChanged := false;
End;

Procedure TMemoExStrings.ReLine;
Var
  L                 : integer;
Begin
  inc(FMemoEx.FUpdateLock);
  Try
    If FParaLinesCount = 0 Then
      L := FMemoEx.FCaretX
    Else
      L := Length(ParaStrings[FParaLinesCount - 1]);
    While FMemoEx.FCaretY > FParaLinesCount - 1 Do
    Begin
      //      if FParaLinesCount > 0 then TReLineUndo.Create(FMemoEx, L, FMemoEx.FCaretY, #13#10);
      If FParaLinesCount > 0 Then
        TReLineUndo.Create(FMemoEx, L, FParaLinesCount - 1, #13#10);
      L := 0;
      Add('');
    End;
    If FMemoEx.FCaretX > Length(ParaStrings[FMemoEx.FCaretY]) Then
    Begin
      L := FMemoEx.FCaretX - Length(ParaStrings[FMemoEx.FCaretY]);
      TReLineUndo.Create(FMemoEx, Length(ParaStrings[FMemoEx.FCaretY]),
        FMemoEx.FCaretY, Spaces(L));
      PutParaString(FMemoEx.FCaretY, ParaStrings[FMemoEx.FCaretY] + Spaces(L));
    End;
  Finally
    dec(FMemoEx.FUpdateLock);
  End;
End;

Procedure TMemoExStrings.SetLockText(Const Text: String);
Begin
  inc(FMemoEx.FUpdateLock);
  Try

    Inherited SetTextStr(FMemoEx.ExpandTabs(Text));

  Finally
    dec(FMemoEx.FUpdateLock);
  End;
End;

Procedure TMemoExStrings.SetInternalParaStr(Index: integer; Const Value:
  String);
Begin
  inc(FMemoEx.FUpdateLock);
  Try
    PutParaString(Index, Value);
  Finally
    dec(FMemoEx.FUpdateLock);
  End;
End;

Procedure TMemoExStrings.SetInternal(Index: integer; Const Value: String);
Begin
  inc(FMemoEx.FUpdateLock);
  Try
    Put(Index, Value);
  Finally
    dec(FMemoEx.FUpdateLock);
  End;
End;

{ TEditorClient }

Function TEditorClient.GetCanvas: TCanvas;
Begin
  Result := FMemoEx.Canvas;
End;

Function TEditorClient.Left: integer;
Begin
  Result := FMemoEx.GutterWidth + 2;
End;

Function TEditorClient.Height: integer;
Begin
  Result := FMemoEx.ClientHeight;
End;

Function TEditorClient.Width: integer;
Begin
  Result := Max(FMemoEx.ClientWidth - Left, 0);
End;

Function TEditorClient.ClientWidth: integer;
Begin
  Result := Width;
End;

Function TEditorClient.ClientHeight: integer;
Begin
  Result := Height;
End;

Function TEditorClient.ClientRect: TRect;
Begin
  Result := Bounds(Left, Top, Width, Height);
End;

Function TEditorClient.BoundsRect: TRect;
Begin
  Result := Bounds(0, 0, Width, Height);
End;

{ TGutter }

Constructor TGutter.Create;
Begin
  FFont := TFont.Create;
  With FFont Do
  Begin
    Name := 'MS Sans Serif';
    Size := 8;
    Color := clWindowText;
  End;
  FDrawBitmap := TBitmap.Create;
End;

Destructor TGutter.Destroy;
Begin
  FFont.Free;
  FDrawBitmap.Free;
  Inherited;
End;

Procedure TGutter.Invalidate;
Begin
  Paint;
End;

Procedure TGutter.Paint;
Var
  iR                : integer;
  ECR               : TRect;
  BX, EX, BY, EY    : integer;
  iTextHeight       : Integer;
  sVAlue            : String;
Begin
  If FMemoEx.FGutterWidth > 0 Then
  Begin
    With FDrawBitmap, Canvas Do
    Begin
      Brush.Style := bsSolid;
      Brush.Color := FMemoEx.FGutterColor;
      FillRect(Bounds(0, 0, Width, Height));
      Pen.Width := 1;
      Pen.Color := FMemoEx.Color;
      MoveTo(Width - 2, 0);
      LineTo(Width - 2, Height);
      Pen.Width := 2;
      MoveTo(Width + 1, 0);
      LineTo(Width + 1, Height);
      Pen.Width := 1;
      Pen.Color := clGray;
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height);
    End;
    FMemoEx.GutterPaint(FDrawBitmap.Canvas);
  End
  Else
    With FDrawBitmap, Canvas Do
    Begin
      Brush.Style := bsSolid;
      Brush.Color := FMemoEx.Color;
      FillRect(Bounds(0, 0, Width, Height));
    End;
  FMemoEx.Canvas.Draw(0, 0, FDrawBitmap);

  If FMemoEx.bPaintLineNumbers Then
  Begin
    ECR := FMemoEx.EditorClient.Canvas.ClipRect;

    If FMemoEx.FAllRepaint Then
      ECR := FMemoEx.EditorClient.BoundsRect;

    BX := ECR.Left Div FMemoEx.FCellRect.Width - 1;
    EX := ECR.Right Div FMemoEx.FCellRect.Width + 1;
    BY := ECR.Top Div FMemoEx.FCellRect.Height;
    EY := ECR.Bottom Div FMemoEx.FCellRect.Height + 1;

    FMemoEx.Canvas.Font.Color := clBlack;
    FMemoEx.Canvas.Brush.Style := bsClear;

    iTextHeight := FMemoEx.Canvas.TextHeight('A');

    For iR := BY To EY Do
    Begin
      sVAlue := inttostr(iR + FMemoEx.TopRow +1);
      FMemoEx.Canvas.TextOut(
        FMemoEx.GutterWidth - FMemoEx.Canvas.TextWidth(sVAlue) - 4,
        (IR - BY) * iTextHeight,
        sVAlue);
    End;
  End;
End;

{*********************** TCustomMemoEx ***********************}

Constructor TCustomMemoEx.Create(AOwner: TComponent);
Var
  Item              : TMenuItem;
Begin
  Inherited Create(AOwner);

  ControlStyle := [csCaptureMouse, csClickEvents {, csOpaque}, csDoubleClicks,
  csReplicatable];

  //=============================================  MHS+
  FMenu := TPopupmenu.Create(Self);
  FMenu.OnPopup := OnMenuPopup;

  Item := TMenuitem.Create(FMenu);
  Item.Caption := 'Rückgängig';
  Item.ShortCut := TextToShortCut('Strg+Z');
  Item.Tag := it_Undo;
  Item.OnClick := OnMenuClick;
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := '-';
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := 'Ausschneiden';
  Item.ShortCut := TextToShortCut('Strg+X');
  Item.Tag := it_Cut;
  Item.OnClick := OnMenuClick;
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := 'Kopieren';
  Item.ShortCut := TextToShortCut('Strg+C');
  Item.Tag := it_Copy;
  Item.OnClick := OnMenuClick;
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := 'Einfügen';
  Item.ShortCut := TextToShortCut('Strg+V');
  Item.Tag := it_Paste;
  Item.OnClick := OnMenuClick;
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := 'Löschen';
  Item.ShortCut := TextToShortCut('Strg+Entf');
  Item.Tag := it_Delete;
  Item.OnClick := OnMenuClick;
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := '-';
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := 'Alles markieren';
  Item.ShortCut := TextToShortCut('Strg+A');
  Item.Tag := it_SelectAll;
  Item.OnClick := OnMenuClick;
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := '-';
  FMenu.Items.Add(Item);

  Item := TMenuitem.Create(FMenu);
  Item.Caption := 'Suchen';
  Item.ShortCut := TextToShortCut('Strg+F');
  Item.Tag := it_Search;
  Item.OnClick := OnMenuClick;
  FMenu.Items.Add(Item);

  //=============================================  MHS

  FInsertMode := true;
  FReadOnly := false;
  FWantTabs := true;
  FLines := TMemoExStrings.Create;
  FLines.FMemoEx := Self;
  FKeyboard := TKeyboard.Create;
  FRows := 1;
  FCols := 1;

  LastXCoord := 0;

  FUndoBuffer := TUndoBuffer.Create;
  FUndoBuffer.FMemoEx := Self;
  FGroupUndo := true;

  FDrawBitmap := TBitmap.Create;

  FFont := TFont.Create;
  With FFont Do
  Begin
    Name := 'Times New Roman';
    Size := 10;
    Pitch := fpFixed;
    OnChange := FontChanged;
  End;

  FTabPos := Nil;
  FCharWidth := Nil;
  SetMax_X(512);

  FRightMarginVisible := true;
  FRightMargin := 600;
  FRealRightMargin := FRightMargin;
  FRightMargin := 0;
  FBorderStyle := bsSingle;
  Ctl3d := true;
  Height := 150;
  Width := 250;
  ParentColor := false;
  Cursor := crIBeam;
  TabStop := true;
  FTabSize := 4;
  FIndentSize := 4;
  FAutoIndentSize := 0;
  FSmartTab := True;
  FSmartIndent := True;
  FBackspaceUnindents := True;
  FAutoIndent := True;
  FKeepTrailingBlanks := False;
  FCursorBeyondEOF := False;
  FCursorBeyondEOL := False;

  FWordWrap := true;

  FScrollBars := ssBoth;
  scbHorz := TMemoExScrollBar.Create;
  scbVert := TMemoExScrollBar.Create;
  scbVert.Kind := sbVertical;
  scbHorz.OnScroll := ScrollBarScroll;
  scbVert.OnScroll := ScrollBarScroll;

  Color := clWindow;
  FGutterColor := clBtnFace;
  FclSelectBC := clHighLight;
  FclSelectFC := clHighLightText;
  FRightMarginColor := clSilver;

  EditorClient := TEditorClient.Create;
  EditorClient.FMemoEx := Self;
  FGutter := TGutter.Create;
  FGutter.FMemoEx := Self;

  FLeftCol := 0;
  FTopRow := 0;
  FSelected := false;
  FCaretX := 0;
  FCaretY := 0;

  timerScroll := TTimer.Create(Self);
  timerScroll.Enabled := false;
  timerScroll.Interval := 50;
  timerScroll.OnTimer := ScrollTimer;

  SelAttrs_Size := 0;

  mouse_down := false;
  double_clicked := false;
  mouse_dragged := false;
  gutter_clicked := false;

  FSimpleBeginLine := true;

  FKeyboard.SetDefLayout;
  FCompletion := TCompletion.Create2(Self);

  FFixedTextFontColor := clBlue;        // MHS
  FLinesOKFontColor := clRed;           // MHS

  FFixedText := '';                     // MHS
  FLinesOK := 0;                        // MHS

End;

Destructor TCustomMemoEx.Destroy;
Begin
  FLines.Free;
  scbHorz.Free;
  scbVert.Free;
  EditorClient.Free;
  FKeyboard.Free;
  FUndoBuffer.Free;
  FCompletion.Free;
  FGutter.Free;
  FDrawBitmap.Free;
  FFont.Free;

  SelAttrs := Nil;
  FTabPos := Nil;
  FCharWidth := Nil;

  FMenu.Free;

  Inherited Destroy;
End;

//==============================================================================
// MHS+
// Umrechnung einer x-Bildschirm-Koordinate in Spalten-Koordinate
//==============================================================================

Function TCustomMemoEx.Coord2Col(S: String; Coord: Integer): Integer;
Var
  Col               : integer;
Begin
  Col := 0;
  While (Col < length(s)) And (Canvas.TextWidth(copy(s, 1, Col)) < Coord -
    Canvas.TextWidth(copy(s, Col, 1)) Div 2) Do
  Begin
    inc(Col);
  End;
  result := Col;
End;
//==============================================================================
// MHS+
// Umrechnung einer Spalte in x-Koordinate
//==============================================================================

Function TCustomMemoEx.Col2Coord(S: String; Col: Integer): Integer;
Begin
  result := Canvas.TextWidth(copy(s, 1, Col));
End;
//==============================================================================
// MHS+
// Zeilenbreite als Koordinate
//==============================================================================

Function TCustomMemoEx.LineWidth(S: String): Integer;
Begin
  result := Canvas.TextWidth(S);
End;

//==============================================================================

Procedure TCustomMemoEx.OnMenuPopup(Sender: TObject);
Begin

  FMenu.Items[0].Enabled := self.CanUndo; // UNDO
  FMenu.Items[2].Enabled := self.FSelectedText; // CUT
  FMenu.Items[3].Enabled := self.FSelectedText; // COPY
  FMenu.Items[4].Enabled := self.CanPaste; // PASTE
  FMenu.Items[5].Enabled := self.FSelectedText; // Delete

End;

Procedure TCustomMemoEx.OnMenuClick(Sender: TObject);
Begin
  Case TMenuItem(sender).tag Of
    it_Copy: self.CopyToClipboard;
    it_Cut: self.CutToClipboard;
    it_Paste: self.PasteFromClipboard;
    it_Delete: self.ClearSelection;
    it_Undo: self.Undo;
    it_SelectAll: self.SelectAll;
    it_Search: self.Search;
  End;
End;

Procedure TCustomMemoEx.Search;
Var
  FRMSearch         : TFRMSearch;
Begin
  FRMSearch := TFRMSearch.create(Nil);
  FRMSearch.edSearch.Text := FSearchText;

  If FRMSearch.ShowModal = mrOk Then
  Begin
    FSearchText := FRMSearch.edSearch.Text;

    If FRMSearch.rgStartSearch.itemindex = 1 Then
    Begin
      Caretx := 0;
      Carety := 0;
    End;

    DoSearch;
  End;

  FRMSearch.Free;
End;

Procedure TCustomMemoEx.DoSearch;
Var
  y                 : Integer;
  x                 : Integer;
  Startx            : Integer;
Begin
  Startx := Caretx + 2;
  x := 0;
  y := CaretY;

  While (y <= FLines.FParaLinesCount - 1) And (x = 0) Do
  Begin
    x := pos(Lowercase(FSearchText),
      Lowercase(copy(FLines.GetParaString(y), Startx,
      length(FLines.GetParaString(y)))));

    If x = 0 Then
      Inc(y)
    Else
      x := x + Startx;
    startx := 1;
  End;

  If x > 0 Then
  Begin
    PaintCaret(false);
    SetCaret(X - 2, Y);
    PaintCaret(true);
  End
  Else If messagedlg('Textende erreicht !!' + #10#13 +
    'Soll ab Textanfang weiter gesucht werden?', mtConfirmation, [mbYes, mbNo],
    0)
    = mrYes Then
  Begin
    Caretx := 0;
    Carety := 0;
    DoSearch;
  End;

End;

//==============================================================================
//==============================================================================
//==============================================================================
//==============================================================================

Procedure TCustomMemoEx.Invalidate;
Begin
  If (csLoading In ComponentState) Then
    exit;
  If FUpdateLock = 0 Then
    Inherited;
End;

Procedure TCustomMemoEx.Loaded;
Begin
  Inherited Loaded;
  scbVertWidth := GetSystemMetrics(SM_CXVSCROLL);
  scbHorzHeight := GetSystemMetrics(SM_CYHSCROLL);
  NextClipViewer := SetClipboardViewer(Handle);
  UpdateEditorSize;
  Changed;
  SelectionChanged;
  ClipboardChanged;
  FModified := false;
  FCompletion.UpdateAutoChange;
End;

Procedure TCustomMemoEx.CreateParams(Var Params: TCreateParams);
Const
  BorderStyles      : Array[TBorderStyle] Of cardinal = (0, WS_BORDER);
  ScrollStyles      : Array[TScrollStyle] Of cardinal = (0, WS_HSCROLL,
    WS_VSCROLL,
    WS_HSCROLL Or WS_VSCROLL);
Begin
  Inherited CreateParams(Params);
  With Params Do
  Begin
    Style := Style Or BorderStyles[FBorderStyle] Or ScrollStyles[FScrollBars];
    If NewStyleControls And Ctl3D And (FBorderStyle = bsSingle) Then
    Begin
      Style := Style And Not WS_BORDER;
      ExStyle := ExStyle Or WS_EX_CLIENTEDGE;
    End;
    WindowClass.Style := WindowClass.Style And Not (CS_HREDRAW Or CS_VREDRAW);
  End;
End;

Procedure TCustomMemoEx.Resize;
Begin
  If Not (csLoading In ComponentState) Then
  Begin
    UpdateEditorSize;
    Invalidate;
  End;
End;

Procedure TCustomMemoEx.CreateWnd;
Begin
  Inherited CreateWnd;
  If FScrollBars In [ssHorizontal, ssBoth] Then
    scbHorz.Handle := Handle;
  If FScrollBars In [ssVertical, ssBoth] Then
    scbVert.Handle := Handle;
  FAllRepaint := true;
End;

Procedure TCustomMemoEx.SetBorderStyle(Value: TBorderStyle);
Begin
  If FBorderStyle <> Value Then
  Begin
    FBorderStyle := Value;
    RecreateWnd;
  End;
End;

Procedure TCustomMemoEx.PaintSelection;
Var
  iR                : integer;
Begin
  For iR := FUpdateSelBegY To FUpdateSelEndY Do
    PaintLine(iR, -1, -1);
End;

Procedure TCustomMemoEx.SetUnSelected;
Begin
  If FSelected Then
  Begin
    FSelectedText := false;
    TUnselectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
      FSelEndX, FSelEndY);
    PaintSelection;
  End;
End;

Function IsRectEmpty(R: TRect): boolean;
Begin
  Result := (R.Top = R.Bottom) And (R.Left = R.Right);
End;

Function TCustomMemoEx.CalcCellRect(Const X, Y: integer): TRect;
Var
  s                 : String;
Begin

  If Y + FTopRow < 0 Then
    s := FLines.ParaStrings[0]
  Else If Y + FTopRow < FLines.GetParaLineCount Then
    s := FLines.ParaStrings[Y + FTopRow]
  Else
    s := FLines.ParaStrings[FLines.Count - 1];

  Result := Bounds(
    //EditorClient.Left + X * FCellRect.Width + 1,  //MHS-

    EditorClient.Left + Col2Coord(s, X) + 1, //MHS+

    EditorClient.Top + Y * FCellRect.Height,
    FCellRect.Width,
    FCellRect.Height)
End;

Procedure TCustomMemoEx.Paint;
Var
  iR                : integer;
  ECR               : TRect;
  BX, EX, BY, EY    : integer;
Begin

  If FUpdateLock > 0 Then
    exit;

  PaintCaret(false);

  ECR := EditorClient.Canvas.ClipRect;
  OffsetRect(ECR, -FGutterWidth, 0);
  If FAllRepaint Then
    ECR := EditorClient.BoundsRect;
  BX := ECR.Left Div FCellRect.Width - 1;
  EX := ECR.Right Div FCellRect.Width + 1;
  BY := ECR.Top Div FCellRect.Height;
  EY := ECR.Bottom Div FCellRect.Height + 1;
  For iR := BY To EY Do
  Begin
    PaintLine(FTopRow + iR, FLeftCol + BX, FLeftCol + EX);
  End;

  FGutter.Paint;
  PaintCaret(true);

  FAllRepaint := false;
End;

Procedure TCustomMemoEx.BeginUpdate;
Begin
  inc(FUpdateLock);
End;

Procedure TCustomMemoEx.EndUpdate;
Begin
  If FUpdateLock = 0 Then
    Exit;                               { Error ? }
  dec(FUpdateLock);
  If FUpdateLock = 0 Then
  Begin
    FAllRepaint := true;
    UpdateEditorSize(false);
    StatusChanged;
    Invalidate;
  End;
End;

{
  Adjust FTabPos and FCharWidth depending on maximum line length.
}

Procedure TCustomMemoEx.SetMax_X(Const Value: integer);
Begin
  Max_X := Value;
  SetLength(FTabPos, Max_X);
  SetLength(FCharWidth, Max_X);
End;

{
  Refresh all parameters.
  FullUpdate is true when recounting of cell and draw bitmap size needed.
}

Procedure TCustomMemoEx.UpdateEditorSize(Const FullUpdate: boolean = true; Const
  RepaintGutter: boolean = true);
Const
  BiggestSymbol     = 'W';
Var
  i                 : integer;
Begin
  If (csLoading In ComponentState) Then
    exit;

  If FullUpdate Then
  Begin
    EditorClient.Canvas.Font := Font;

    EditorClient.Canvas.Font.Style := [];

    FCellRect.Width := Max(1, EditorClient.Canvas.TextWidth(BiggestSymbol));
    FCellRect.Height := Max(1, EditorClient.Canvas.TextHeight(BiggestSymbol));

    //for i := 0 to Max_X - 1 do FCharWidth[i] := FCellRect.Width;

    For i := 0 To Max_X - 1 Do
      FCharWidth[i] := canvas.TextWidth(chr(i));

    EditorClient.Canvas.Font := Font;
    FDrawBitmap.Canvas.Font.Assign(EditorClient.Canvas.Font);
    FDrawBitmap.Canvas.Brush.Assign(EditorClient.Canvas.Brush);
    FDrawBitmap.Width := Width;
    FDrawBitmap.Height := FCellRect.Height;
    FGutter.FDrawBitmap.Width := FGutterWidth + 2;
    FGutter.FDrawBitmap.Height := Height;
  End;

  FVisibleColCount := Trunc(EditorClient.ClientWidth / FCellRect.Width);
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;

  FVisibleRowCount := Trunc(EditorClient.ClientHeight / FCellRect.Height);
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FCols := -1;
  FRows := -1;
  Rows := FLines.ParaLineCount;
  If FWordWrap Then
    Cols := FRealRightMargin
  Else
    Cols := Max_X;
  If RepaintGutter Then
    FGutter.Invalidate;
End;

//==============================================================================
// Hier wird der Text, der fiexierte Text, der rechte Rand, etc
// in den sichtbaren Bereich "gemalt"
//==============================================================================

Procedure TCustomMemoEx.PaintLine(Const Line: integer; ColBeg, ColEnd: integer);
Var
  Ch                : String;
  R, R1             : TRect;
  F, k, x, i, j, iC, jC, SL, MX, PX, PY: integer;
  T, S              : String;
  LA, LB            : TLineAttr;
  FL                : PParagraphList;
Begin

  If (Line < FTopRow) Or (Line > FTopRow + FVisibleRowCount) Or (FUpdateLock > 0)
    Then
    exit;

  If ColBeg < FLeftCol Then
    ColBeg := FLeftCol;

  {  if (ColEnd < 0) or (ColEnd > FLeftCol + FVisibleColCount + 1) then   // MHS
       ColEnd := FLeftCol + FVisibleColCount + 1;}

  ColEnd := 30000;
  ColEnd := Min(ColEnd, Max_X - 1);

  j := 0;
  i := ColBeg;

  FDrawBitmap.Canvas.Brush.Color := Color;
  FDrawBitmap.Canvas.FillRect(Bounds(EditorClient.Left, 0, EditorClient.Width,
    FCellRect.Height));

  If (Line > -1) And (Line < FLines.ParaLineCount) Then
    With FDrawBitmap Do
    Begin
      T := FLines.GetParagraphByIndex(Line, PY, PX);
      S := FLines.ParaStrings[Line];

      If Not FWordWrap Then
      Begin
        iC := ColBeg;
        jC := ColEnd;
      End
      Else
      Begin
        iC := 0;
        jC := length(T);
      End;
      GetLineAttr(PY, Line, PX, length(S), iC, jC, T);

      SL := Length(S);
      If SL > ColEnd Then
        MX := ColEnd
      Else
        MX := SL;

      If (FStripInvisible) And (FReadOnly) And (ColBeg > 0) Then
      Begin
        FL := FLines.FList;
        x := PX + ColBeg;
        If x >= SelAttrs_Size Then
          x := SelAttrs_Size - 1;
        For k := PX To x Do
          If FL^[PY].FAttrs[k].FC = FL^[PY].FAttrs[k].BC Then
            inc(j);
      End;

      While i < MX Do
        With Canvas Do
        Begin
          iC := i + 1;
          jC := iC + 1;
          If iC <= SL Then
            Ch := S[iC]
          Else
            Ch := ' ';
          If (iC + PX > SelAttrs_Size) Or (jC + PX > SelAttrs_Size) Then
            break;
          LA := FLines.FList^[PY].FAttrs[iC + PX - 1];
          If SelAttrs[iC + PX - 1] Then
            With LA Do
            Begin
              FC := FclSelectFC;
              BC := FclSelectBC;
            End;

          While (jC <= MX) And (jC + PX <= SelAttrs_Size) Do
          Begin
            LB := FLines.FList^[PY].FAttrs[jC + PX - 1];
            If SelAttrs[jC + PX - 1] Then
              With LB Do
              Begin
                FC := FclSelectFC;
                BC := FclSelectBC;
              End;
            If CompareMem(@LA, @LB, sizeof(TLineAttr)) Then
            Begin
              If jC <= SL Then
                Ch := Ch + S[jC]
              Else
                Ch := Ch + ' ';
              inc(jC);
            End
            Else
              break;
          End;

          // ggf. Zeichnen des Textes
          If (Not ((LA.BC = LA.FC) And (FStripInvisible))) Or (Not ReadOnly)
            Then
          Begin
            Brush.Color := LA.BC;
            Font.Color := LA.FC;
            Font.Style := LA.Style;

            // Entscheidung in welcher Farbe die Zeile dargestellt werden soll;
            If (Line >= FLinesOK) And (FLinesOK > 0) Then
            Begin                       // MHS+
              Font.Color := FLinesOKFontColor; // MHS+
              Font.Style := [];         // MHS+
            End;                        // MHS+

            R := CalcCellRect(i - j, Line - FTopRow);
            R.Left := R.Left - FLeftCol * FCellRect.Width;

            // Darstellung des Textes:
            FDrawBitmap.Canvas.TextOut(R.Left, 0, ch); // MHS+

          End
          Else
            inc(j, length(Ch));
          i := jC - 1;
        End;
    End;

  // Das ist der sichtbare Bereich
  R := Bounds(EditorClient.Left,
    (Line - FTopRow) * FCellRect.Height,
    (FVisibleColCount + 2) * FCellRect.Width,
    FCellRect.Height);

  // Darstellung des fixen Textes in der 0.Zeile:
  If (line = 0) Then
  Begin                                 //MHS+
    FDrawBitmap.Canvas.font.Color := FFixedTextFontColor; //MHS+
    FDrawBitmap.Canvas.Brush.color := clWhite; //MHS+
    FDrawBitmap.Canvas.Font.Style := [fsBold]; //MHS+

    FDrawBitmap.Canvas.TextOut(self.GutterWidth - FLeftCol * FCellRect.Width +
      2, 0, FFixedText);                //MHS+

    FFixedTextWidth := FDrawBitmap.Canvas.TextWidth(FFixedText);
    FDrawBitmap.Canvas.Font := Font;
  End;

  // Darstellung des rechten Randes als Linie:
  If FRightMarginVisible { and (FRealRightMargin > FLeftCol) and (FRealRightMargin < FLastVisibleCol + 3) }
    Then
    With FDrawBitmap.Canvas Do
    Begin
      Pen.Color := FRightMarginColor;
      F := 2 + FRealRightMargin - FLeftCol * FCellRect.Width + self.GutterWidth;
      MoveTo(F, 0);
      LineTo(F, FCellRect.Height);
    End;

  // den Kram wirklich zeichnen
  BitBlt(EditorClient.Canvas.Handle, R.Left, R.Top, R.Right - R.Left,
    FCellRect.Height,
    FDrawBitmap.Canvas.Handle, R.Left, 0,
    SRCCOPY);

End;

{
  Get line attributes.
  Line contains paragraph index.
  LineIdx contains line index in ParaStrings.
  LineOffs contains line offset from the paragraph beginning.
  LineLen length of line.
}

Procedure TCustomMemoEx.GetLineAttr(Line, LineIdx, LineOffs, LineLen, ColBeg,
  ColEnd: integer; Const ALine: String);

  Procedure ChangeSelectedAttr;

    Procedure DoChange(Const iBeg, iEnd: integer);
    Var
      i             : integer;
    Begin
      If (iBeg + LineOffs < SelAttrs_Size) And (iEnd + LineOffs < SelAttrs_Size)
        Then
        For i := iBeg + LineOffs To iEnd + LineOffs Do
          SelAttrs[i] := true;
    End;

  Begin
    If SelAttrs_Size > 0 Then
      ZeroMemory(@SelAttrs[0], SelAttrs_Size);
    If Not FSelected Then
      exit;
    If (LineIdx = FSelBegY) And (LineIdx = FSelEndY) Then
      DoChange(FSelBegX, Min(LineLen - 1, FSelEndX - 1 + integer(FInclusive)))
    Else
    Begin
      If LineIdx = FSelBegY Then
        DoChange(FSelBegX, LineLen - 1);
      If (LineIdx > FSelBegY) And (LineIdx < FSelEndY) Then
        DoChange(0, LineLen - 1);
      If LineIdx = FSelEndY Then
        DoChange(0, Min(LineLen - 1, FSelEndX - 1 + integer(FInclusive)));
    End
  End;

Var
  i                 : integer;
Begin
  If SelAttrs_Size <> length(ALine) + 1 Then
  Begin
    SelAttrs_Size := length(ALine) + 1;
    SetLength(SelAttrs, SelAttrs_Size);
  End;
  ChangeSelectedAttr;

  If FLines.FList^[Line].FChanged Then
  Begin
    With FLines.FList^[Line] Do
    Begin
      SetLength(FAttrs, SelAttrs_Size);
      FAttrs[0].FC := Font.Color;
      FAttrs[0].Style := Font.Style;
      FAttrs[0].BC := Color;
      FAttrs[0].ex_style := ME_EX_STYLE_DEFAULT;
      For i := 1 To SelAttrs_Size - 1 Do
        Move(FAttrs[0], FAttrs[i], sizeof(TLineAttr));
    End;
    GetAttr(ALine, Line, ColBeg, ColEnd);

    If ALine <> '' Then
      If Assigned(FOnGetLineAttr) Then
        FOnGetLineAttr(Self, ALine, Line, SelAttrs, FLines.FList^[Line].FAttrs);

    ChangeAttr(Line, ColBeg, ColEnd);
    FLines.FList^[Line].FChanged := false;
  End;
End;
//==============================================================================
//   MHS+
//   Identifiers für die Autovervollständigung werden ggf. farbig dargestellt
//==============================================================================

Procedure TCustomMemoEx.GetAttr(ALine: String; Line, ColBeg, ColEnd: integer);
Var
  x1, x2, k, col, Idents: Integer;
  AWord             : String;
Begin
  If FCompletion.ColoringIdent Then
  Begin
    For Idents := 0 To FCompletion.Identifiers.Count - 1 Do
    Begin
      col := 0;
      x1 := 0;
      x2 := 0;
      Repeat
        aWord := trim(GetWordOnPosEx(ALine, col, x1, x2));
        col := x2 + 1;

        If lowercase(FCompletion.Identifiers.Names[Idents]) = lowercase(AWord)
          Then
        Begin
          For k := x1 - 1 To x2 - 2 Do
          Begin
            FLines.FList^[Line].FAttrs[k].FC := FCompletion.FIdentifierColor;
            FLines.FList^[Line].FAttrs[k].BC := Color;
            FLines.FList^[Line].FAttrs[k].Style := [];
            FLines.FList^[Line].FAttrs[k].ex_style := 0;
          End;
        End;

      Until x2 >= length(ALine);

    End;
  End;
End;

Procedure TCustomMemoEx.ChangeAttr(Line, ColBeg, ColEnd: integer);
Begin
End;

Procedure TCustomMemoEx.ScrollBarScroll(Sender: TObject; ScrollCode:
  TScrollCode; Var ScrollPos: integer);
Begin
  Case ScrollCode Of
    scLineUp..scPageDown, scTrack:
      Begin
        If Sender = scbVert Then
          Scroll(true, ScrollPos)
        Else If Sender = scbHorz Then
          Scroll(false, ScrollPos);
      End;
  End;
End;

Procedure TCustomMemoEx.Scroll(Const Vert: boolean; Const ScrollPos: integer);
Var
  R, RClip, RUpdate : TRect;
  OldFTopRow        : integer;
Begin
  If FUpdateLock = 0 Then
  Begin
    PaintCaret(false);
    If Vert Then
    Begin                               {Vertical Scroll}
      OldFTopRow := FTopRow;
      FTopRow := ScrollPos;
      If Abs((OldFTopRow - ScrollPos) * FCellRect.Height) < EditorClient.Height
        Then
      Begin
        R := EditorClient.ClientRect;
        R.Bottom := R.Top + CellRect.Height * (FVisibleRowCount + 1);
        RClip := R;
        ScrollDC(
          EditorClient.Canvas.Handle,   // handle of device context
          0,                            // horizontal scroll units
          (OldFTopRow - ScrollPos) * FCellRect.Height, // vertical scroll units
          R, // address of structure for scrolling rectangle
          RClip, // address of structure for clipping rectangle
          0,                            // handle of scrolling region
          @RUpdate // address of structure for update rectangle
          );
        InvalidateRect(Handle, @RUpdate, false);
      End
      Else
        Invalidate;
      Update;
    End
    Else                                {Horizontal Scroll}
    Begin
      FLeftCol := ScrollPos;
      Invalidate;
    End;
  End
  Else                                  { FUpdateLock > 0 }
  Begin
    If Vert Then
      FTopRow := ScrollPos
    Else
      FLeftCol := ScrollPos;
  End;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  If FUpdateLock = 0 Then
  Begin
    FGutter.Invalidate;
    PaintCaret(true);
  End;
  If Assigned(FOnScroll) Then
    FOnScroll(Self);
End;

//==============================================================================
// Zeichne den Cursor
//==============================================================================

Procedure TCustomMemoEx.PaintCaret(Const bShow: boolean);
Var
  R                 : TRect;
Begin
  // Cursor darf nicht in den fixen Textbereich
  If (FCarety = 0) And (FCaretx < FFixedTextLength) Then
  Begin                                 // MHS+
    FCaretX := FFixedTextLength;        // MHS+
    If Assigned(FOnChangeStatus) Then
      FOnChangeStatus(Self);            // MHS+
  End;                                  // MHS+

  // zeichne den Windows-Cursor, wenn der Focus auf der Komponente ist
  If Not bShow Then
    HideCaret(Handle)
  Else If Focused Then
  Begin
    R := CalcCellRect(FCaretX, FCaretY - FTopRow);
    Windows.SetCaretPos(R.Left - FLeftCol * FCellRect.Width - 1, R.Top + 1);
    ShowCaret(Handle)
  End;
End;

Procedure TCustomMemoEx.SetCaretInternal(X, Y: integer);
Var
  R                 : TRect;
Begin
  If (X = FCaretX) And (Y = FCaretY) Then
    exit;

  If Not FCursorBeyondEOF Then
    Y := Min(Y, FLines.ParaLineCount - 1);
  Y := Max(Y, 0);
  X := Min(X, Max_X);
  X := Max(X, 0);

  If Y < FTopRow Then
    SetLeftTop(FLeftCol, Y)
  Else If Y > Max(FLastVisibleRow, 0) Then
    SetLeftTop(FLeftCol, Y - FVisibleRowCount + 1);

  If X < 0 Then
    X := 0;

  If X < FLeftCol Then
    SetLeftTop(X, FTopRow);

  { if X > FLastVisibleCol then                          // MHS-
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  SetLeftTop(X - FVisibleColCount , FTopRow);}// MHS-

  R := CalcCellRect(X, Y - FTopRow);
  R.Left := R.Left - FLeftCol * FCellRect.Width; // MHS+
  Windows.SetCaretPos(R.Left - 1, R.Top + 1);
  If (FCaretX <> X) Or (FCaretY <> Y) Then
  Begin
    FCaretX := X;
    FCaretY := Y;
    StatusChanged;
  End;
  FCaretX := X;
  FCaretY := Y;
End;

Procedure TCustomMemoEx.SetCaret(X, Y: integer);
Begin
  If (X = FCaretX) And (Y = FCaretY) Then
    exit;
  TCaretUndo.Create(Self, FCaretX, FCaretY);
  SetCaretInternal(X, Y);
  If FUpdateLock = 0 Then
    StatusChanged;
End;

Procedure TCustomMemoEx.SetCaretPosition(Const index, Pos: integer);
Begin
  If index = 0 Then
    SetCaret(Pos, FCaretY)
  Else
    SetCaret(FCaretX, Pos)
End;

Procedure TCustomMemoEx.KeyDown(Var Key: Word; Shift: TShiftState);
Var
  Form              : TCustomForm;
  Com               : word;
Begin
  If FCompletion.FVisible Then
    If FCompletion.DoKeyDown(Key, Shift) Then
      exit
    Else
  Else
    FCompletion.FTimer.Enabled := false;

  If (Key = VK_TAB) And ((Shift = []) Or (Shift = [ssShift])) Then
    If ((FReadOnly) Or (Not FWantTabs)) Then
    Begin
      Form := GetParentForm(Self);
      If Assigned(Form) Then
      Begin
        Key := 0;
        If Shift = [] Then
          Form.Perform(WM_NEXTDLGCTL, 0, 0)
        Else
          Form.Perform(WM_NEXTDLGCTL, 1, 0);
      End;
      exit;
    End;

  If WaitSecondKey Then
  Begin
    Com := FKeyboard.Command2(Key1, Shift1, Key, Shift);
    WaitSecondKey := false;
    IgnoreKeyPress := true;
  End
  Else
  Begin
    Inherited KeyDown(Key, Shift);
    Key1 := Key;
    Shift1 := Shift;
    Com := FKeyboard.Command(Key, Shift);

    If Com = twoKeyCommand Then
    Begin
      IgnoreKeyPress := true;
      WaitSecondKey := true;
    End
    {Else
      IgnoreKeyPress := Com > 0};
  End;
  If (Com > 0) And (Com <> twoKeyCommand) Then
  Begin
    Key := 0;
    Shift := [];
    Command(Com);
  End;
  If (Com = ecBackSpace) Then
    FCompletion.DoKeyPress(#8);
End;

{
  We have to be sure the current line exists and position in it exists too.
}

Procedure TCustomMemoEx.ReLine;
Begin
  FLines.ReLine;
  UpdateEditorSize;
  scbVert.Position := FTopRow;
End;                                    { ReLine }

Procedure TCustomMemoEx.KeyPress(Var Key: Char);
Begin
  If IgnoreKeyPress Then
  Begin
    IgnoreKeyPress := false;
    exit
  End;
  If FReadOnly Then
    exit;
  PaintCaret(false);
  Inherited KeyPress(Key);

  Command(ord(Key));

  PaintCaret(true);
End;

Function AutoChangeCompare(Item1, Item2: pointer): integer;
Var
  i, j              : integer;
Begin
  i := length(PAutoChangeWord(Item1)^.OldWord);
  j := length(PAutoChangeWord(Item2)^.OldWord);
  If i = j Then
    Result := 0
  Else If i > j Then
    Result := 1
  Else
    Result := -1;
End;

//==============================================================================
// Einfügen eines Zeichens
//==============================================================================

Procedure TCustomMemoEx.InsertChar(Const Key: Char);

  Function GetAutoChangeWord(Const CurrentWord: String; Var NewWord: String):
      boolean;
  Var
    i, j, k         : integer;
    s, t            : String;
  Begin
    Result := false;
    t := DoChangeCase(CurrentWord, ME_CASE_CONVERT_LOWER);
    j := length(t);
    For i := 0 To FCompletion.FAutoChangeList.Count - 1 Do
    Begin
      s := PAutoChangeWord(FCompletion.FAutoChangeList[i])^.OldWord;
      k := length(s);
      If j < k Then
        break
      Else If j = k Then
        If t = s Then
        Begin
          Result := true;
          NewWord := PAutoChangeWord(FCompletion.FAutoChangeList[i])^.NewWord;
          break;
        End;
    End;
  End;

Var
  S                 : String;
  T, old_str, new_str: String;
  k1, k2, str_pos   : integer;
  AutoChanged, AddKeyToNewStr: boolean;
  oldChar           : String;
  i, _X, _Y, Y      : integer;
  b                 : boolean;          //  flag showing we should draw a lot
Begin
  ReLine;
  Case Key Of
    #32..#255:
      Begin
        If Not HasChar(Key, RAEditorCompletionChars) Then
          FCompletion.DoKeyPress(Key);
        Begin
          ClearSelection;
          FLines.Caret2Paragraph(FCaretX, FCaretY, FParaY, FParaX);
          new_str := '';
          old_str := '';
          str_pos := 0;
          If (Key In _AutoChangePunctuation) And
            (FCompletion.FAutoChangeList.Count > 0) Then
          Begin
            S := FLines[FParaY];
            AutoChanged := false;
            AddKeyToNewStr := false;
            str_pos := FParaX - 1;
            //  k1 -- length of the smallest substring for auto-change
            //  k2 -- length of the longest substring for auto-change
            k1 :=
              length(PAutoChangeWord(FCompletion.FAutoChangeList[0])^.OldWord);
            k2 :=
              length(PAutoChangeWord(FCompletion.FAutoChangeList[FCompletion.FAutoChangeList.Count - 1])^.OldWord);
            //  cycling while we are not at the paragraph beginning and
            //  haven't got substring longer than k2
            While (str_pos > -1) And (FParaX - str_pos <= k2) Do
            Begin
              If FParaX - str_pos >= k1 Then
              Begin
                old_str := System.Copy(S, str_pos + 1, FParaX - str_pos);
                AutoChanged := GetAutoChangeWord(old_str, new_str);  //  should we change substring w/o punctuation sign?
                If Not AutoChanged Then
                  AutoChanged := GetAutoChangeWord(old_str + Key, new_str)  //  again: should we change substring w/o punctuation sign?
                Else
                  AddKeyToNewStr := true;
                If AutoChanged Then
                  //  substring can be changed if it is at the beginning of the line
                  //  or punctuation sign before it.
                  If ((str_pos > 0) And (S[str_pos] In _AutoChangePunctuation))
                    Or (str_pos = 0) Then
                    break
                  Else
                    AutoChanged := false;
              End;
              dec(str_pos);
            End;
            If AutoChanged Then
              If AddKeyToNewStr Then
                //  if we found a replacement, should the punctuation sign
                //  be replace too?
                If GetAutoChangeWord(Key, T) Then
                  new_str := new_str + T
                Else
                  new_str := new_str + Key
              Else
            Else
            Begin
              //  should we change entered sign?
              AutoChanged := GetAutoChangeWord(Key, new_str);
              If AutoChanged Then
              Begin
                str_pos := FParaX;      //  yes.
                old_str := '';
              End;
            End;
          End
          Else
            AutoChanged := false;
          If AutoChanged Then
          Begin
            {
              str_pos -- replacement substring start coordinate
              S       -- paragraph
              old_str -- substring to replace
              new_str -- replacement substring
            }

            //  undo for all replacement
            BeginCompound;
            TCaretUndo.Create(Self, FCaretX, FCaretY);
            FLines.Paragraph2Caret(FParaY, str_pos, _X, _Y);
            If (length(old_str) + integer(Not FInsertMode) > 0) And (length(S) >
              0) Then
            Begin
              If FInsertMode Then
                T := old_str
              Else
                T := old_str + S[FParaX + 1];
              TDeleteUndo.Create(Self, _X, _Y, T);
            End;
            If length(new_str) > 0 Then
              TInsertUndo.Create(Self, _X, _Y, new_str);
            EndCompound;

            //  replacement
            k1 := FLines.Paragraphs[FParaY].FCount;
            System.Delete(S, str_pos + 1, length(old_str) + integer(Not
              FInsertMode));
            System.Insert(new_str, S, str_pos + 1);
            FLines.Internal[FParaY] := S;
            B := k1 <> FLines.Paragraphs[FParaY].FCount;
            FParaX := str_pos + length(new_str);
          End
          Else
          Begin
            //  simple symbol inserting
            S := FLines.ParaStrings[FCaretY];
            If FInsertMode Then
            Begin
              TInsertUndo.Create(Self, FCaretX, FCaretY, Key);
              Insert(Key, S, FCaretX + 1);
            End
            Else
            Begin
              If FCaretX + 1 <= Length(S) Then
              Begin
                oldChar := S[FCaretX + 1];
                S[FCaretX + 1] := Key;
              End
              Else
              Begin
                oldChar := '';
                S := S + Key;
              End;
              TOverwriteUndo.Create(Self, FCaretX, FCaretY, oldChar, Key);
            End;

            Y := FCaretY;
            i := FLines.Paragraphs[FParaY].FCount;
            FLines.InternalParaStrings[Y] := S;
            inc(FParaX);

            B := i <> FLines.Paragraphs[FParaY].FCount;
          End;

          //  repanting of the paragraph.
          i := RepaintParagraph(FCaretY);

          //  full paragraph repainting needed only if number
          //  of wrapped lines in paragraph was changed.
          If B Then
          Begin
            UpdateEditorSize(false);
            RedrawFrom(i + 1);
          End;

          FLines.Paragraph2Caret(FParaY, FParaX, _X, _Y);
          SetCaretInternal(_X, _Y);

          Changed;
        End;
        If HasChar(Key, RAEditorCompletionChars) Then
          FCompletion.DoKeyPress(Key);
      End;
  End;
End;

Procedure TCustomMemoEx.RedrawFrom(YFrom: integer);
Var
  i                 : integer;
Begin
  For i := YFrom - 1 To FLastVisibleRow + 1 Do
  Begin
    PaintLine(i, -1, -1);
  End;
End;

Function TCustomMemoEx.RepaintParagraph(LineIndex: integer): integer;
Var
  P, PI, i, j, k    : integer;
Begin
  FLines.Index2ParaIndex(LineIndex, P, PI);
  j := LineIndex - PI;
  k := j + FLines.Paragraphs[P].FCount - 1;
  If j < FTopRow - 1 Then
    j := FTopRow - 1;
  j := Max(0, j);
  If k > FLastVisibleRow + 1 Then
    k := FLastVisibleRow + 1;
  Result := k;
  For i := j To k Do
    PaintLine(i, -1, -1);
End;

Function TCustomMemoEx.IsUndoEmpty: boolean;
Begin
  Result := FUndoBuffer.FPtr < 0;
End;

Function TCustomMemoEx.YinBounds(AY: integer): boolean;
Begin
  Result := (AY > -1) And (AY < FLines.ParaLineCount);
End;

Function TCustomMemoEx.DoChangeCase(Const st: String;
  Conversion: byte): String;
Begin
  If Assigned(FOnCaseConversion) Then
    Result := FOnCaseConversion(Self, Conversion, st)
  Else
    Case Conversion Of
      ME_CASE_CONVERT_UPPER:
        Result := ANSIUpperCase(st);
      ME_CASE_CONVERT_LOWER:
        Result := ANSILowerCase(st);
    Else
      Result := ANSIChangeCase(st);
    End;
End;

Type
  EComplete = Class(EAbort);

  //==============================================================================
  // der Kommando-Interpreter
  //==============================================================================

Procedure TCustomMemoEx.Command(ACommand: TEditCommand);
Var
  X, Y              : integer;
  CaretUndo         : boolean;

Type
  TPr = Procedure Of Object;

  Procedure DoAndCorrectXY(Pr: TPr);
  Begin
    Pr;
    X := FCaretX;
    Y := FCaretY;
    CaretUndo := false;
  End;

  Function Com(Const Args: Array Of TEditCommand): boolean;
  Var
    i               : integer;
  Begin
    Result := true;
    For i := 0 To High(Args) Do
      If Args[i] = ACommand Then
        exit;
    Result := false;
  End;

  Procedure SetSel1(X, Y: integer);
  Begin
    SetSel(X, Y);
    CaretUndo := false;
  End;

  Procedure SetSelText1(S: String);
  Begin
    SelText := S;
    CaretUndo := false;
  End;

  Procedure Complete;
  Begin
    Raise EComplete.Create('');
  End;

  Function BeginLineCoordinate: integer;
  Var
    z               : String;
    i               : integer;
  Begin
    If (Not FSimpleBeginLine) And (FLines.ParaLineCount > 0) Then
    Begin
      z := FLines.ParaStrings[Y];
      i := length(z) - length(_TrimLeft(z));
      If X = i Then
        Result := 0
      Else
        Result := i;
    End
    Else
      Result := 0;
  End;

Var
  F, _Y, indentX, indentY1, indentY2: integer;
  S, S2, T          : String;
  B, isIndent       : boolean;
  iBeg, iEnd, i     : integer;
Begin
  X := FCaretX;
  Y := FCaretY;
  CaretUndo := true;
  PaintCaret(false);

  { macro recording }
  If FRecording And Not Com([ecRecordMacro, ecBeginCompound]) And (FCompound = 0)
    Then
    FMacro := FMacro + Char(Lo(ACommand)) + Char(Hi(ACommand));

  Try
    Try
      Case ACommand Of

        { caret movements }

        ecLeft, ecRight, ecSelLeft, ecSelRight:
          Begin
            If Com([ecSelLeft, ecSelRight]) And Not FSelected Then
              SetSel1(X, Y);
            B := Com([ecLeft, ecSelLeft]);
            If B Then
              dec(X)
            Else
              inc(X);

            If (Not FCursorBeyondEOL) And (YinBounds(Y)) Then
            Begin
              _Y := 0;
              If (B) And (X < 0) Then
                _Y := -1
              Else If (Not B) And (X > length(FLines.ParaStrings[Y])) Then
                _Y := 1;
              If (_Y <> 0) And (YinBounds(Y + _Y)) Then
              Begin
                Y := Y + _Y;
                If B Then
                  X := length(FLines.ParaStrings[Y])
                Else
                  X := 0;
              End
              Else If X > length(FLines.ParaStrings[Y]) Then
                X := length(FLines.ParaStrings[Y]);
            End
            Else If Not FCursorBeyondEOL Then
              X := 0;

            If Com([ecSelLeft, ecSelRight]) Then
              SetSel1(X, Y)
            Else
              SetUnSelected;
          End;
        ecUp, ecDown, ecSelUp, ecSelDown:
          If (Com([ecUp, ecSelUp]) And (Y > 0)) Or (Com([ecDown, ecSelDown]) And
            (Y < FRows - 1)) Or ((ACommand = ecDown) And (FCursorBeyondEOF))
              Then
          Begin
            If Com([ecSelUp, ecSelDown]) And Not FSelected Then
              SetSel1(X, Y);

            If Com([ecUp, ecSelUp]) Then
            Begin
              dec(Y);
              // berechne die korrespondierende x-Position der vorherigen y-Position
              If (FLines.GetParaString(Y + 1)) <> '' Then //MHS+
                LastXCoord := Col2Coord(FLines.GetParaString(Y + 1), X) + 3;  //MHS+
              // setze Cursor auf die berechnete Position
              X := Coord2Col(FLines.GetParaString(Y), LastXCoord); //MHS+
            End
            Else
            Begin
              inc(Y);
              // berechne die korrespondierende x-Position der vorherigen y-Position
              If (FLines.GetParaString(Y - 1)) <> '' Then //MHS+
                LastXCoord := Col2Coord(FLines.GetParaString(Y - 1), X) + 3;  //MHS+

              // setze Cursor auf die berechnete Position
              X := Coord2Col(FLines.GetParaString(Y), LastXCoord); //MHS+
            End;

            If (Not FCursorBeyondEOL) And (YinBounds(Y)) Then
              If X > length(FLines.ParaStrings[Y]) Then
                X := length(FLines.ParaStrings[Y]);

            If Com([ecSelUp, ecSelDown]) Then
              SetSel1(X, Y)
            Else
              SetUnSelected;
          End;
        ecPrevWord, ecSelPrevWord:
          If FLines.ParaLineCount > 0 Then
          Begin
            S := FLines.ParaStrings[Y];
            If X > length(S) Then
              X := length(S);
            If X = 0 Then
              If Y > 0 Then
              Begin
                dec(Y);
                X := length(FLines.ParaStrings[Y]);
              End
              Else
            Else
            Begin
              If (ACommand = ecSelPrevWord) And Not FSelected Then
                SetSel1(FCaretX, FCaretY);
              B := false;
              For F := X - 1 Downto 0 Do
                If B Then
                  If (S[F + 1] In Separators) Then
                  Begin
                    X := F + 1;
                    break;
                  End
                  Else
                Else If Not (S[F + 1] In Separators) Then
                  B := true;

              If X = FCaretX Then
                X := 0;
              If ACommand = ecSelPrevWord Then
                SetSel1(X, Y)
              Else
                SetUnselected;
              If (Not B) And (X = 0) And (Y > 0) Then
              Begin
                {
                  Jump to the next line automatically.
                }
                FCaretX := X;
                Command(ACommand);
                Complete;
              End;
            End;
          End;
        ecNextWord, ecSelNextWord:
          If FLines.ParaLineCount > 0 Then
          Begin
            If X >= length(FLines.ParaStrings[Y]) Then
            Begin
              If Y < FLines.ParaLineCount - 1 Then
              Begin
                inc(Y);
                X := 0;
                If length(FLines.ParaStrings[Y]) > 0 Then
                  If FLines.ParaStrings[Y][X + 1] = #32 Then
                  Begin
                    FCaretX := X;
                    FCaretY := Y;
                    Command(ACommand);
                    Complete;
                  End;
              End;
            End
            Else
            Begin
              If (ACommand = ecSelNextWord) And Not FSelected Then
                SetSel1(FCaretX, FCaretY);
              S := FLines.ParaStrings[Y];
              B := false;
              For F := X To Length(S) - 1 Do
                If B Then
                  If Not (S[F + 1] In Separators) Then
                  Begin
                    X := F;
                    break;
                  End
                  Else
                Else If (S[F + 1] In Separators) Then
                  B := true;

              If X = FCaretX Then
              Begin
                B := X <> length(S);
                X := length(S);
              End;
              If ACommand = ecSelNextWord Then
                SetSel1(X, Y)
              Else
                SetUnselected;
              If (Not B) And (X = length(S)) And (Y < FLines.ParaLineCount - 1)
                Then
              Begin
                {
                  Jump to the next line automatically.
                }
                FCaretX := X;
                Command(ACommand);
                Complete;
              End;
            End;
          End;
        ecScrollLineUp, ecScrollLineDown,
          ecScrollPageUp, ecScrollPageDown:
          Begin
            If Not ((ACommand = ecScrollLineDown) And (Y >= FLines.ParaLineCount
              - 1) And (Y = FTopRow)) Then
            Begin
              Case ACommand Of
                ecScrollLineUp:
                  F := -1;
                ecScrollLineDown:
                  F := 1;
                ecScrollPageUp:
                  F := -scbVert.LargeChange;
              Else
                F := scbVert.LargeChange;
              End;
              scbVert.Position := scbVert.Position + F;
              Scroll(true, scbVert.Position);
            End;
            If Y < FTopRow Then
              Y := FTopRow
            Else If Y > FLastVisibleRow Then
              Y := FLastVisibleRow;
          End;
        ecBeginLine, ecSelBeginLine, ecBeginDoc, ecSelBeginDoc,
          ecEndLine, ecSelEndLine, ecEndDoc, ecSelEndDoc:
          Begin
            If Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc])
              And Not FSelected Then
              SetSel1(FCaretX, Y);

            If Com([ecBeginLine, ecSelBeginLine]) Then
              X := BeginLineCoordinate
            Else If Com([ecBeginDoc, ecSelBeginDoc]) Then
            Begin
              X := 0;
              Y := 0;
              SetLeftTop(x, y);
            End
            Else If Com([ecEndLine, ecSelEndLine]) Then
              If FLines.ParaLineCount > 0 Then
                X := Length(FLines.ParaStrings[Y])
              Else
                X := 0
            Else If Com([ecEndDoc, ecSelEndDoc]) Then
              If FLines.ParaLineCount > 0 Then
              Begin
                Y := FLines.ParaLineCount - 1;
                X := Length(FLines.ParaStrings[Y]);

                SetLeftTop(X - FVisibleColCount, Y - FVisibleRowCount + 1
                  { div 2});
              End;

            If Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc])
              Then
              SetSel1(X, Y)
            Else
              SetUnSelected;
          End;
        ecNextPage, ecPrevPage, ecSelNextPage, ecSelPrevPage:
          Begin
            If Com([ecPrevPage, ecSelPrevPage]) Then
              _Y := -1
            Else
              _Y := 1;
            If Com([ecSelNextPage, ecSelPrevPage]) Then
            Begin
              BeginUpdate;
              SetSel1(X, Y);
            End;
            Y := Y + _Y * FVisibleRowCount;
            If (Not CursorBeyondEOF) Or (ACommand = ecSelNextPage) Then
              If Y > FLines.ParaLineCount - 1 Then
                Y := FLines.ParaLineCount - 1;
            If Y < 0 Then
              Y := 0;

            If Y <= (_Y * scbVert.LargeChange) Then
            Begin
              scbVert.Position := scbVert.Position + _Y * scbVert.LargeChange;
              Scroll(true, scbVert.Position);
            End;

            If (Not FCursorBeyondEOL) And (YinBounds(Y)) Then
              If X > length(FLines.ParaStrings[Y]) Then
                X := length(FLines.ParaStrings[Y]);
            If Com([ecSelNextPage, ecSelPrevPage]) Then
            Begin
              SetSel1(X, Y);
              EndUpdate;
            End
            Else
              SetUnSelected;

          End;
        ecSelWord:
          If Not FSelected And (GetWordOnPosEx(FLines.ParaStrings[Y] + ' ', X +
            1, iBeg, iEnd) <> '') Then
          Begin
            SetSel1(iBeg - 1, Y);
            SetSel1(iEnd - 1, Y);
            X := iEnd - 1;
          End;
        ecWindowTop:
          Begin
            Y := FTopRow;
            If (Not FCursorBeyondEOL) And (YinBounds(Y)) Then
              If X > length(FLines.ParaStrings[Y]) Then
                X := length(FLines.ParaStrings[Y]);
            SetUnSelected;
          End;
        ecWindowBottom:
          Begin
            Y := FTopRow + FVisibleRowCount - 1;
            If (Not FCursorBeyondEOL) And (YinBounds(Y)) Then
              If X > length(FLines.ParaStrings[Y]) Then
                X := length(FLines.ParaStrings[Y]);
            SetUnSelected;
          End;
        { editing }
        ecCharFirst..ecCharLast:
          If Not FReadOnly Then
          Begin
            InsertChar(Char(ACommand - ecCharFirst));
            Complete;
          End;
        ecInsertPara:
          If Not FReadOnly Then
          Begin
            ClearSelection;
            ReLine;
            X := FCaretX;
            Y := FCaretY;
            FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
            S := FLines[FParaY];
            S2 := Copy(S, FParaX + 1, length(S));
            T := S2;
            If Assigned(FOnBreakLine) Then
              FOnBreakLine(Self, S, S2);
            If S2 = T Then
            Begin
              {
                Just paragraph insertion.
              }
              F := 0;
              If FAutoIndent Then
              Begin
                If FSmartIndent Then
                  X := GetTabStop(0, Y, tsAutoIndent, true)
                Else
                  X := FAutoIndentSize;
                If _Trim(S2) > '' Then
                Begin
                  i := length(S2) - length(_TrimLeft(S2));
                  If X - i > 0 Then
                  Begin
                    S2 := Spaces(X - i) + S2;
                    F := X - i;
                  End;
                End;
              End
              Else
                X := 0;
              BeginCompound;
              TInsertUndo.Create(Self, FCaretX, FCaretY, #13#10 + Spaces(F));
              CaretUndo := false;
              FLines.Internal[FParaY] := Copy(S, 1, FParaX);
              FLines.Insert(FParaY + 1, S2);
              FLines.Paragraph2Caret(FParaY + 1, 0, F, Y);
              EndCompound;
            End
            Else
            Begin
              {
                User has changed wrapped line.
              }
              T := Copy(S, 1, FParaX) + #13#10 + S2 + #13#10;
              F := FLines.GetParaOffs(FParaY);
              S2 := FLines.Text;
              System.Delete(S2, F + 1, length(S) + 2); //  delete old text.
              System.Insert(T, S2, F + 1); //  and insert a new one.

              FLines.Paragraph2Caret(FParaY, 0, F, _Y);

              CaretUndo := false;
              BeginCompound;
              TCaretUndo.Create(Self, FCaretX, FCaretY);
              TDeleteUndo.Create(Self, 0, _Y, S + #13#10);
              TInsertUndo.Create(Self, 0, _Y, T);
              EndCompound;

              FLines.SetLockText(S2);

              FLines.Paragraph2Caret(FParaY + 1, 0, F, Y);

              X := 0;
            End;
            UpdateEditorSize(false);
            F := RepaintParagraph(Max(0, Y - 1));
            RedrawFrom(F + 1);
            Changed;
          End;
        ecBackword:
          If (FCaretY = 0) And (FCaretX <= FFixedTextLength) Then
          Begin
          End
          Else If Not FReadOnly Then
          Begin
            If length(FLines.ParaStrings[Y]) > 0 Then
            Begin
              Command(ecBeginCompound);
              Command(ecBeginUpdate);
              Command(ecSelPrevWord);
              Command(ecDeleteSelected);
              Command(ecEndUpdate);
              Command(ecEndCompound);
            End
            Else
              Command(ecBackspace);
            Complete;
          End;
        ecBackspace:
          If (FCaretY = 0) And (FCaretX <= FFixedTextLength) And Not FSelected
            Then
          Begin                         //MHS+
          End
          Else If Not FReadOnly Then
          Begin
            If FSelected Then
            Begin
              DoAndCorrectXY(ClearSelection);
              Changed;
            End
            Else
            Begin
              ReLine;
              X := FCaretX;
              Y := FCaretY;

              FLines.Caret2Paragraph(X, Y, FParaY, FParaX);

              If (X > 0) Then
              Begin
                {
                  Delete in the middle of the line.
                }

                If FBackspaceUnindents Then
                  X := GetBackStop(FCaretX, FCaretY)
                Else
                  X := FCaretX - 1;
                S := Copy(FLines.ParaStrings[FCaretY], X + 1, FCaretX - X);

                dec(FParaX, length(S));

                F := FLines.Paragraphs[FParaY].FCount;

                FLines.InternalParaStrings[Y] := Copy(FLines.ParaStrings[Y], 1,
                  X) +
                  Copy(FLines.ParaStrings[Y], FCaretX + 1,
                  Length(FLines.ParaStrings[Y]));
                FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                TBackspaceUndo.Create(Self, X, Y, S);
                CaretUndo := false;

                B := F <> FLines.Paragraphs[FParaY].FCount;
                F := RepaintParagraph(Y);
                If B Then
                Begin
                  UpdateEditorSize(false);
                  RedrawFrom(F + 1);
                End;
                Changed;
              End
              Else If Y > 0 Then
              Begin
                {
                  Pasting of the lines.
                }
                If FParaX > 0 Then
                Begin
                  {
                    Pasting of the wrapped lines in paragraph.
                  }
                  T := FLines[FParaY];
                  S := Copy(T, FParaX, 1);

                  System.Delete(T, FParaX, 1);
                  FLines.Internal[FParaY] := T;
                  dec(FParaX);
                  FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                  TBackspaceUndo.Create(Self, X, Y, S);
                  CaretUndo := false;
                End
                Else If FParaY > 0 Then
                Begin
                  {
                    Pasting of the paragraphs.
                  }
                  inc(FUpdateLock);

                  S := FLines[FParaY - 1];
                  S2 := FLines[FParaY];
                  If Assigned(FOnConcatLine) Then
                    FOnConcatLine(Self, S, S2);

                  CaretUndo := false;
                  FLines.Paragraph2Caret(FParaY - 1, 0, F, _Y);

                  BeginCompound;
                  TCaretUndo.Create(Self, X, Y);
                  TDeleteUndo.Create(Self, 0, _Y, FLines[FParaY - 1] + #13#10 +
                    FLines[FParaY] + #13#10);
                  TInsertUndo.Create(Self, 0, _Y, S + S2 + #13#10);
                  EndCompound;

                  FLines.Internal[FParaY - 1] := S + S2;
                  FLines.Delete(FParaY);

                  dec(FUpdateLock);
                  FLines.Paragraph2Caret(FParaY - 1, length(S), X, Y);
                End
                Else
                  Complete;
                UpdateEditorSize(false);
                F := RepaintParagraph(Y);
                RedrawFrom(F + 1);
                Changed;
              End;
            End;
          End;
        ecDelete:
          If Not FReadOnly Then
          Begin
            If FLines.ParaLineCount = 0 Then
              FLines.Add('');
            If FSelected Then
            Begin
              DoAndCorrectXY(ClearSelection);
              Changed;
            End
            Else
            Begin
              ReLine;
              X := FCaretX;
              Y := FCaretY;
              FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
              If X < Length(FLines.ParaStrings[Y]) Then
              Begin
                {
                  Deleting in the middle of the line.
                }
                TDeleteUndo.Create(Self, FCaretX, FCaretY,
                  FLines.ParaStrings[Y][X + 1]);
                CaretUndo := false;

                F := FLines.Paragraphs[FParaY].FCount;

                FLines.InternalParaStrings[Y] := Copy(FLines.ParaStrings[Y], 1,
                  X) +
                  Copy(FLines.ParaStrings[Y], X + 2,
                  Length(FLines.ParaStrings[Y]));
                FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                B := F <> FLines.Paragraphs[FParaY].FCount;
                F := RepaintParagraph(Y);
                If B Then
                Begin
                  UpdateEditorSize(false);
                  RedrawFrom(F + 1);
                End;
                Changed;
              End
              Else If (Y >= 0) And (Y <= FLines.ParaLineCount - 2) Then
              Begin
                {
                  Pasting of the lines.
                }
                S := FLines[FParaY];
                If FParaX < length(S) Then
                Begin
                  {
                    Pasting of the wrapped lines.
                  }
                  TDeleteUndo.Create(Self, FCaretX, FCaretY, System.Copy(S,
                    FParaX + 1, 1));
                  CaretUndo := false;

                  System.Delete(S, FParaX + 1, 1);
                  FLines.Internal[FParaY] := S;
                  FLines.Paragraph2Caret(FParaY, FParaX, X, Y);
                End
                Else
                Begin
                  {
                    Pasting of the paragraphs.
                  }
                  inc(FUpdateLock);

                  S := FLines[FParaY];
                  S2 := FLines[FParaY + 1];
                  If Assigned(FOnConcatLine) Then
                    FOnConcatLine(Self, S, S2);

                  CaretUndo := false;
                  FLines.Paragraph2Caret(FParaY, 0, F, _Y);

                  BeginCompound;
                  TCaretUndo.Create(Self, X, Y);
                  TDeleteUndo.Create(Self, 0, _Y, FLines[FParaY] + #13#10 +
                    FLines[FParaY + 1] + #13#10);
                  TInsertUndo.Create(Self, 0, _Y, S + S2 + #13#10);
                  EndCompound;

                  FLines.Internal[FParaY] := S + S2;
                  FLines.Delete(FParaY + 1);

                  dec(FUpdateLock);
                  FLines.Paragraph2Caret(FParaY, length(S), X, Y);
                End;
                UpdateEditorSize(false);
                F := RepaintParagraph(FCaretY);
                RedrawFrom(F + 1);
                Changed;
              End;
            End;
          End;
        ecTab, ecBackTab:
          If Not FReadOnly Then
            If FSelected Then
              If ACommand = ecTab Then
                PostCommand(ecIndent)
              Else
                PostCommand(ecUnindent)
            Else
            Begin
              ReLine;
              X := FCaretX;
              Y := FCaretY;
              X := GetTabStop(FCaretX, FCaretY, tsTabStop, ACommand = ecTab);
              If (ACommand = ecTab) And FInsertMode Then
              Begin
                S := FLines.ParaStrings[FCaretY];
                FLines.Caret2Paragraph(FCaretX, FCaretY, FParaY, FParaX);
                S2 := Spaces(X - FCaretX);

                TInsertTabUndo.Create(Self, FCaretX, FCaretY, S2);
                CaretUndo := false;

                Insert(S2, S, FCaretX + 1);

                F := FLines.Paragraphs[FParaY].FCount;

                FLines.InternalParaStrings[FCaretY] := S;
                inc(FParaX, X - FCaretX);
                FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                B := F <> FLines.Paragraphs[FParaY].FCount;
                F := RepaintParagraph(FCaretY);
                If B Then
                Begin
                  UpdateEditorSize(false);
                  RedrawFrom(F + 1);
                End;
                Changed;
              End;
            End;
        ecIndent, ecUnindent:
          If (Not FReadOnly) And (FSelected) Then
          Begin
            isIndent := ACommand = ecIndent;
            FLines.Caret2Paragraph(FSelBegX, FSelBegY, indentY1, indentX);
            FLines.Caret2Paragraph(FSelEndX, FSelEndY, indentY2, indentX);
            If indentX = 0 Then
            Begin
              dec(indentY2);
              indentX := length(FLines[indentY2]);
              FLines.Paragraph2Caret(indentY2, indentX, FSelEndX, FSelEndY);
            End;

            BeginCompound;
            TSelectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX,
              FSelBegY, FSelEndX, FSelEndY);
            If isIndent Then
            Begin
              TIndentUndo.Create(Self, indentY1, indentY2, FIndentSize);
              EndCompound;
            End;
            CaretUndo := false;

            F := FIndentSize;
            If Not isIndent Then
              F := (-1) * F;
            S := Spaces(FIndentSize);
            B := false;
            FLines.BeginUpdate;
            If isIndent Then
            Begin
              B := true;
              For iBeg := indentY1 To indentY2 Do
                FLines[iBeg] := S + FLines[iBeg];
            End
            Else
              For iBeg := indentY1 To indentY2 Do
                If (S > '') And (S[1] = ' ') Then
                Begin
                  B := true;
                  T := FLines[iBeg];
                  i := length(T) - length(_TrimLeft(T));
                  If i > FIndentSize Then
                    i := FIndentSize;
                  System.Delete(T, 1, i);
                  FLines[iBeg] := T;
                  TUnindentUndo.Create(Self, iBeg, i);
                End;
            FLines.EndUpdate;
            If Not isIndent Then
              If B Then
                EndCompound
              Else
              Begin
                UndoBuffer.Delete;
                UndoBuffer.Delete;
              End;
            If B Then
            Begin
              inc(FSelBegX, F);
              If FSelBegX < 0 Then
                FSelBegX := 0;
              inc(FSelEndX, F);
              If FSelEndX < 0 Then
                FSelEndX := length(FLines[indentY2]);
              If (Y >= indentY1) And (Y <= indentY2) Then
                inc(X, F);
              Changed;
            End;
            FSelected := true;
          End;
        ecChangeInsertMode:
          Begin
            FInsertMode := Not FInsertMode;
            StatusChanged;
          End;
        ecClipBoardCut:
          If Not FReadOnly Then
            DoAndCorrectXY(CutToClipboard);
        ecClipBoardCopy:
          CopyToClipboard;
        ecClipBoardPaste:
          If Not FReadOnly Then
            DoAndCorrectXY(PasteFromClipboard);
        ecDeleteSelected:
          If Not FReadOnly And FSelected Then
            DoAndCorrectXY(ClearSelection);

        ecDeleteWord:
          If Not FReadOnly Then
            If length(FLines.ParaStrings[Y]) = 0 Then
              Command(ecDelete)
            Else
            Begin
              Command(ecBeginCompound);
              Command(ecBeginUpdate);
              Command(ecSelNextWord);
              Command(ecDeleteSelected);
              Command(ecEndUpdate);
              Command(ecEndCompound);
              Complete;
            End;
        ecDeleteLine:
          If (Not FReadOnly) And (Y >= 0) And (Y <= FLines.ParaLineCount - 1)
            Then
          Begin
            FLines.Index2ParaIndex(Y, F, _Y);
            B := (Not FWordWrap) Or (_Y = FLines.Paragraphs[F].FCount - 1);
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginLine);
            Command(ecSelEndLine);
            Command(ecDeleteSelected);
            // âîçìîæíî íóæíî ñêëåèòü ñòðîêè
            If B Then
              Command(ecDelete);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            Complete;
          End;
        ecSelAll:
          Begin
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginDoc);
            Command(ecSelEndDoc);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            SelectionChanged;
            Complete;
          End;
        ecToUpperCase:
          If (Not FReadOnly) And (FSelected) Then
            SelText := DoChangeCase(SelText, ME_CASE_CONVERT_UPPER);
        ecToLowerCase:
          If (Not FReadOnly) And (FSelected) Then
            SelText := DoChangeCase(SelText, ME_CASE_CONVERT_LOWER);
        ecChangeCase:
          If (Not FReadOnly) And (FSelected) Then
            SelText := DoChangeCase(SelText, ME_CASE_CONVERT_INVERT);
        ecUndo:
          If Not FReadOnly Then
          Begin
            FUndoBuffer.Undo;
            PaintCaret(true);
            Complete;
          End;
        ecRedo:
          If Not FReadOnly Then
          Begin
            FUndoBuffer.Redo;
            PaintCaret(true);
            Complete;
          End;
        ecBeginCompound:
          BeginCompound;
        ecEndCompound:
          EndCompound;

        ecSetBookmark0..ecSetBookmark9:
          ChangeBookMark(ACommand - ecSetBookmark0, true);
        ecGotoBookmark0..ecGotoBookmark9:
          Begin
            ChangeBookmark(ACommand - ecGotoBookmark0, false);
            X := FCaretX;
            Y := FCaretY;
          End;
        ecInsertMacro0..ecInsertMacroZ:
          If (Assigned(FOnInsertMacro)) And (Not FReadOnly) Then
          Begin
            S := FOnInsertMacro(Self, ACommand - ecInsertMacro0);
            If S = '' Then
              exit;
            InsertTextAtCurrentPos(S);
            PaintCaret(true);
            Complete;
          End;
        ecBlockOpA..ecBlockOpZ:
          If (Not FReadOnly) And (Assigned(FOnBlockOperation)) And (FSelected)
            Then
            SelText := FOnBlockOperation(Self, ACommand - ecBlockOpA, SelText);
        ecCompletionIdentifiers:
          If (Not FReadOnly) And (FCompletion.Enabled) Then
          Begin
            FCompletion.DoCompletion(cmIdentifiers);
            PaintCaret(true);
            Complete;
          End;
        ecCompletionTemplates:
          If (Not FReadOnly) And (FCompletion.Enabled) Then
          Begin
            FCompletion.DoCompletion(cmTemplates);
            PaintCaret(true);
            Complete;
          End;
        ecBeginUpdate:
          BeginUpdate;
        ecEndUpdate:
          EndUpdate;

        ecRecordMacro:
          If FRecording Then
            EndRecord(FDefMacro)
          Else
            BeginRecord;
        ecPlayMacro:
          Begin
            PlayMacro(FDefMacro);
            Complete;
          End;
        ecSaveBlock:
          If (FSelected) And (Assigned(FOnSaveBlock)) Then
            FOnSaveBlock(Self, SelText);
        ecInsertBlock:
          If (Not FReadOnly) And (Assigned(FOnInsertBlock)) Then
          Begin
            If FOnInsertBlock(Self, S) Then
              InsertTextAtCurrentPos(S);
            PaintCaret(true);
            Complete;
          End;
        ecSearch:
          Begin
            self.Search;
            exit;
          End;
        ecSearchNext:
          Begin
            self.DoSearch;
            exit;
          End;

      End;

      If CaretUndo Then
        SetCaret(X, Y)
      Else
        SetCaretInternal(X, Y);
    Except
      On E: EComplete Do                { OK }
        ;
    End;
  Finally
    PaintCaret(true);
  End;
End;

Procedure TCustomMemoEx.PostCommand(ACommand: TEditCommand);
Begin
  PostMessage(Handle, WM_EDITCOMMAND, ACommand, 0);
End;

Procedure TCustomMemoEx.ClipboardChanged;
Begin
  If (csLoading In ComponentState) Or (csDestroying In ComponentState) Then
    exit;
  If Assigned(FOnChangeClipboardState) Then
    FOnChangeClipboardState(Self, IsClipboardFormatAvailable(CF_TEXT) Or
      IsClipboardFormatAvailable(CF_OEMTEXT));
End;

Procedure TCustomMemoEx.CMEnabledChanged(Var Message: TMessage);
Begin
  Inherited;
  Invalidate;
End;

Procedure TCustomMemoEx.WndProc(Var Message: TMessage);
Var
  Form              : TCustomForm;
  pt, temp          : TPoint;
  RC                : TRect;
  DC                : HDC;
Begin
  Case Message.Msg Of
    CM_COLORCHANGED:
      Begin
        Message.Result := 0;
        Invalidate2;
        exit;
      End;
    WM_MOUSEWHEEL:
      Begin
        MouseWheelHandler(Message);
        Message.Result := 0;
        exit;
      End;
    WM_SYSCHAR:
      If Message.wParam = VK_BACK Then
      Begin
        Message.Result := 0;
        exit;
      End;
    WM_SETFOCUS:
      Begin
        Form := GetParentForm(Self);
        If (Form <> Nil) And (Not Form.SetFocusedControl(Self)) Then
          exit;
        CreateCaret(Handle, 0, 2, CellRect.Height - 2);
        PaintCaret(true);
        DoEnter;
      End;
    WM_KILLFOCUS:
      Begin
        If csFocusing In ControlState Then
          exit;
        If FCompletion.FVisible Then
          FCompletion.CloseUp(false);
        DestroyCaret;
        DoExit;
      End;
    WM_GETDLGCODE:
      Begin
        Inherited WndProc(Message);
        TWMGetDlgCode(Message).Result := DLGC_WANTARROWS Or DLGC_WANTCHARS;
        If FWantTabs Then
          TWMGetDlgCode(Message).Result := TWMGetDlgCode(Message).Result Or
            DLGC_WANTTAB;
        exit;
      End;
    WM_HSCROLL:
      Begin
        scbHorz.DoScroll(TWMHScroll(Message));
        exit;
      End;
    WM_VSCROLL:
      Begin
        scbVert.DoScroll(TWMVScroll(Message));
        exit;
      End;
    WM_SETTINGCHANGE:
      Begin
        scbVertWidth := GetSystemMetrics(SM_CXVSCROLL);
        scbHorzHeight := GetSystemMetrics(SM_CYHSCROLL);
      End;
    WM_EDITCOMMAND:
      Begin
        Command(Message.WParam);
        Message.Result := ord(true);
        exit;
      End;
    WM_CHANGECBCHAIN:
      Begin
        Message.Result := 0;
        If TWMChangeCBChain(Message).Remove = NextClipViewer Then
          NextClipViewer := TWMChangeCBChain(Message).Next
        Else
          SendMessage(NextClipViewer, WM_CHANGECBCHAIN,
            TWMChangeCBChain(Message).Remove, TWMChangeCBChain(Message).Next);
        exit;
      End;
    WM_DRAWCLIPBOARD:
      Begin
        ClipboardChanged;
        SendMessage(NextClipViewer, WM_DRAWCLIPBOARD, 0, 0);
        exit;
      End;
    WM_DESTROY:
      ChangeClipboardChain(Handle, NextClipViewer);
    WM_CONTEXTMENU:
      Begin
        pt := SmallPointToPoint(TWMContextMenu(Message).Pos);
        If pt.X < 0 Then
          temp := pt
        Else
          temp := ScreenToClient(pt);
        If PtInRect(ClientRect, temp) Then
          GetWordUnderCursor(temp.X, temp.Y);
      End;
    WM_COPY:
      Begin
        PostCommand(ecClipboardCopy);
        Message.Result := ord(true);
        exit;
      End;
    WM_CUT:
      Begin
        If Not FReadOnly Then
          PostCommand(ecClipboardCut);
        Message.Result := ord(true);
        exit;
      End;
    WM_PASTE:
      Begin
        If Not FReadOnly Then
          PostCommand(ecClipBoardPaste);
        Message.Result := ord(true);
        exit;
      End;
    WM_ERASEBKGND:
      Begin
        Message.Result := 0;
        exit;
      End;
  End;
  Inherited WndProc(Message);
End;

Procedure TCustomMemoEx.InvalidateBookmarks;
Var
  i                 : TBookmarkNum;
Begin
  For i := Low(TBookmarkNum) To High(TBookmarkNum) Do
    FBookmarks[i].Valid := false;
  Invalidate;
End;

Procedure TCustomMemoEx.ChangeBookmark(Const BookMark: TBookMarkNum; Const
  Valid: boolean);

  Procedure SetXY(X, Y: integer);
  Var
    X1, Y1          : integer;
  Begin
    X1 := FLeftCol;
    Y1 := FTopRow;
    If (Y < FTopRow) Or (Y > FLastVisibleRow) Then
      Y1 := Y - (FVisibleRowCount Div 2);
    If (X < FLeftCol) Or (X > FVisibleColCount) Then
      X1 := X - (FVisibleColCount Div 2);
    //    SetLeftTop(X1, Y1);   MHS-
    SetCaret(X, Y);
  End;

Begin
  If Valid Then
    If FBookmarks[Bookmark].Valid And (FBookmarks[Bookmark].Y = FCaretY) Then
      FBookmarks[Bookmark].Valid := false
    Else
    Begin
      FBookmarks[Bookmark].X := FCaretX;
      FBookmarks[Bookmark].Y := FCaretY;
      FBookmarks[Bookmark].Valid := true;
    End
  Else If FBookmarks[Bookmark].Valid Then
    SetXY(FBookmarks[Bookmark].X, FBookmarks[Bookmark].Y);
  BookmarkCnanged(BookMark);
End;

Procedure TCustomMemoEx.BookmarkCnanged(BookMark: integer);
Begin
  FGutter.Invalidate;
End;

Function TCustomMemoEx.GetBookmark(AIndex: integer): TBookmark;
Begin
  If (AIndex < Low(TBookmarkNum)) Or (AIndex > High(TBookmarkNum)) Then
    Raise EListError.CreateFmt(SListIndexError, [AIndex]);
  Result := FBookmarks[AIndex];
End;

Procedure TCustomMemoEx.SetBookmark(AIndex: integer; ABookmark: TBookmark);
Begin
  If (AIndex < Low(TBookmarkNum)) Or (AIndex > High(TBookmarkNum)) Then
    Raise EListError.CreateFmt(SListIndexError, [AIndex]);
  FBookmarks[AIndex] := ABookmark;
End;

Procedure TCustomMemoEx.SelectionChanged;
Begin
  If Not (csLoading In ComponentState) Then
    If Assigned(FOnSelectionChange) Then
      FOnSelectionChange(Self);
End;

Procedure TCustomMemoEx.SetSel(Const ASelX, ASelY: integer);

  Procedure UpdateSelected;
  Var
    iR              : integer;
  Begin
    If FUpdateLock = 0 Then
    Begin
      If (FUpdateSelBegY <> FSelBegY) Or (FUpdateSelBegX <> FSelBegX) Then
        For iR := Min(FUpdateSelBegY, FSelBegY) To Max(FUpdateSelBegY, FSelBegY)
          Do
          PaintLine(iR, -1, -1);

      If (FUpdateSelEndY <> FSelEndY) Or (FUpdateSelEndX <> FSelEndX) Then
        For iR := Min(FUpdateSelEndY, FSelEndY) To Max(FUpdateSelEndY, FSelEndY)
          Do
          PaintLine(iR, -1, -1);

      SelectionChanged;
    End;
  End;

Var
  SelX, SelY        : integer;
Begin
  If ASelX < 0 Then
    SelX := 0
  Else
    SelX := ASelX;
  If ASelY < 0 Then
    SelY := 0
  Else
    SelY := ASelY;

  If Not FSelected Then
  Begin
    FSelStartX := SelX;
    FSelStartY := SelY;
    FSelEndX := SelX;
    FSelEndY := SelY;
    FSelBegX := SelX;
    FSelBegY := SelY;
    FSelected := true;
  End
  Else
  Begin
    TSelectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
      FSelEndX, FSelEndY);

    FUpdateSelBegX := FSelBegX;
    FUpdateSelBegY := FSelBegY;
    FUpdateSelEndX := FSelEndX;
    FUpdateSelEndY := FSelEndY;

    If SelY <= FSelStartY Then
      FSelBegY := SelY;
    If SelY >= FSelStartY Then
      FSelEndY := SelY;

    If (SelY < FSelStartY) Or ((SelY = FSelStartY) And (SelX <= FSelStartX))
      Then
    Begin
      FSelBegX := SelX;
      FSelEndX := FSelStartX;
      FSelEndY := FSelStartY;
    End
    Else If (SelY > FSelStartY) Or ((SelY = FSelStartY) And (SelX >= FSelStartX))
      Then
    Begin
      FSelBegX := FSelStartX;
      FSelBegY := FSelStartY;
      FSelEndX := SelX;
    End;

    If FSelBegY < 0 Then
      FSelBegY := 0;

    // Damit nicht irgendetwas ausserhalb markiert wird!!
    If FSelEndY >= FLines.GetParaLineCount - 1 Then
      FSelEndY := FLines.GetParaLineCount - 1; // MHS+

    FSelected := true;

    If FCompound = 0 Then
      UpdateSelected;

  End;
  If FUpdateSelBegY > FSelBegY Then
    FUpdateSelBegY := FSelBegY;
  If FUpdateSelEndY < FSelEndY Then
    FUpdateSelEndY := FSelEndY;

End;

Procedure TCustomMemoEx.Mouse2Cell(Const X, Y: integer; Var CX, CY: integer);
Var
  s                 : String;
Begin
  CY := (Y - EditorClient.Top) Div FCellRect.Height;

  If cy < 0 Then
    s := FLines.ParaStrings[0]
  Else If cy + FTopRow < FLines.GetParaLineCount Then
    s := FLines.ParaStrings[cy + FTopRow]
  Else
    s := FLines.ParaStrings[FLines.Count - 1];

  CX := Coord2Col(s, X - EditorClient.Left + FLeftCol * FCellRect.Width);  //Round((X - EditorClient.Left) / FCellRect.Width);
End;

//==============================================================================
// berechne die Maus-Koordinaten:
//==============================================================================

Procedure TCustomMemoEx.Mouse2Caret(Const X, Y: integer; Var CX, CY: integer);
Begin
  Mouse2Cell(X, Y, CX, CY);
  If CX < 0 Then
    CX := 0;
  If CY < 0 Then
    CY := 0;
  //CX := CX + FLeftCol;
  CY := CY + FTopRow;
  //if CX > FLastVisibleCol then CX := FLastVisibleCol;    // MHS-
  If CY > FLines.ParaLineCount - 1 Then
    CY := FLines.ParaLineCount - 1;
End;

//==============================================================================
// berechne die Maus-Koordinaten:
//==============================================================================

Procedure TCustomMemoEx.CaretCoord(Const X, Y: integer; Var CX, CY: integer);
Var
  s                 : String;
Begin
  CX := X - FLeftCol;
  CY := Y - FTopRow;
  If CX < 0 Then
    CX := 0;
  If CY < 0 Then
    CY := 0;

  CY := FCellRect.Height * CY;

  s := FLines.ParaStrings[CY + FTopRow];

  //CX := FCellRect.Width * CX;  // MHS-
  CX := Col2Coord(s, CX - EditorClient.Left + FLeftCol * FCellRect.Width);  // MHS+
End;

Function TCustomMemoEx.ExtractStringWithStyle(XX, YY: integer; Const From:
  String; Style: word; Const LineAttrs: TLineAttrs): String;
Var
  i                 : integer;
Begin
  If Style <> ME_EX_STYLE_DEFAULT Then
  Begin
    Result := '';
    If XX <= length(From) Then
      For i := XX Downto 0 Do
        If LineAttrs[i].ex_style = Style Then
          Result := From[i + 1] + Result
        Else
          break;
    For i := XX + 1 To length(From) - 1 Do
      If LineAttrs[i].ex_style = Style Then
        Result := Result + From[i + 1]
      Else
        break;
  End;
End;

{
  We shound count real attribute's offset if invisble stripped.
}

Function TCustomMemoEx.GetAttrDelta(StartFrom, EndTo: integer; Const LineAttrs:
  TLineAttrs): integer;
Var
  i, j              : integer;
Begin
  Result := 0;
  If (ReadOnly) And (FStripInvisible) Then
  Begin
    j := EndTo;
    i := StartFrom;
    While (i <= j) And (i < SelAttrs_Size) Do
    Begin
      If LineAttrs[i].FC = LineAttrs[i].BC Then
      Begin
        inc(Result);
        inc(j);
      End;
      inc(i);
    End;
  End;
End;

Function TCustomMemoEx.DoMouseWheel(Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint): boolean;
Begin
  MouseWheelScroll(WheelDelta);
  Result := Inherited DoMouseWheel(Shift, WheelDelta, MousePos);
End;

Procedure TCustomMemoEx.MouseWheelScroll(Delta: integer);
Var
  i                 : integer;
Begin
  i := Mouse.WheelScrollLines;
  If Delta > 0 Then
    i := -i;
  scbVert.Position := scbVert.Position + i;
  Scroll(true, scbVert.Position);
End;

Procedure TCustomMemoEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
Var
  XX, YY            : integer;
Begin
  If double_clicked Then
  Begin
    double_clicked := false;
    exit;
  End;

  If (Button = mbRight) Then
    Exit;                               // MHS+

  If FCompletion.FVisible Then
    FCompletion.CloseUp(false);
  mouse_down := true;
  SetCapture(Handle);
  mouse_dragged := false;
  gutter_clicked := (X <= FGutterWidth);
  Mouse2Caret(X, Y, XX, YY);
  PaintCaret(false);
  If (Button = mbLeft) And (Not (ssShift In Shift)) Then
    SetUnSelected;
  SetFocus;
  If YinBounds(YY) Then
  Begin
    If Not FCursorBeyondEOL Then
      If XX > length(FLines.ParaStrings[YY]) Then
        XX := length(FLines.ParaStrings[YY]);
    If (ssShift In Shift) And (SelLength = 0) Then
      SetSel(FCaretX, FCaretY);
    SetCaret(XX, YY);
    If ssShift In Shift Then
      SetSel(XX, YY);
  End;
  PaintCaret(true);
  Inherited MouseDown(Button, Shift, X, Y);
End;

Procedure TCustomMemoEx.DblClick;
Var
  i, PY, PX, iBeg, iEnd: integer;
Begin
  double_clicked := true;
  If Assigned(FOnDblClick) Then
    FOnDblClick(Self);
  If FDoubleClickLine Then
  Begin
    PaintCaret(false);
    SetSel(0, FCaretY);
    If FCaretY = FLines.ParaLineCount - 1 Then
    Begin
      SetSel(Length(FLines.ParaStrings[FCaretY]), FCaretY);
      SetCaret(Length(FLines.ParaStrings[FCaretY]), FCaretY);
    End
    Else
    Begin
      SetSel(0, FCaretY + 1);
      SetCaret(0, FCaretY + 1);
    End;
    PaintCaret(true);
  End
  Else If YinBounds(FCaretY) Then
  Begin
    FLines.GetParagraphByIndex(FCaretY, PY, PX);
    i := GetAttrDelta(PX, FCaretX + PX, FLines.Paragraphs[PY].FAttrs);
    If GetWordOnPosEx(FLines.ParaStrings[FCaretY] + ' ', FCaretX + 1 + i, iBeg,
      iEnd) <> '' Then
    Begin
      PaintCaret(false);
      SetSel(iBeg - 1, FCaretY);
      SetSel(iEnd - 1, FCaretY);
      SetCaret(iEnd - 1 - i, FCaretY);
      PaintCaret(true);
    End;
  End;
End;

Procedure TCustomMemoEx.GetWordUnderCursor(X, Y: integer);
Var
  XX, YY, PX, PY, i : integer;
  s                 : String;
Begin
  Mouse2Caret(X, Y, XX, YY);
  If YinBounds(YY) Then
  Begin
    s := FLines.GetParagraphByIndex(YY, PY, PX);
    GetLineAttr(PY, YY, PX, length(s), 0, length(s), s);
    i := XX + PX + GetAttrDelta(PX, XX + PX, FLines.Paragraphs[PY].FAttrs);
    If (i > 0) And (i < SelAttrs_Size) Then
    Begin
      FWordStyleUnderCursor := FLines.Paragraphs[PY].FAttrs[i - 1].ex_style;
      FWordUnderCursor := ExtractStringWithStyle(i, YY, s,
        FWordStyleUnderCursor,
        FLines.Paragraphs[PY].FAttrs);
    End
    Else
    Begin
      FWordUnderCursor := '';
      FWordStyleUnderCursor := ME_EX_STYLE_DEFAULT;
    End;
  End;
End;

Procedure TCustomMemoEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
Begin
  timerScroll.Enabled := false;
  mouse_down := false;
  ReleaseCapture;

  If (Button = mbRight) Then
  Begin
    If Not assigned(self.popupmenu) Then
      FMenu.Popup(Mouse.CursorPos.x, Mouse.CursorPos.y);
    exit;
  End;

  If (Button = mbLeft) And (Not mouse_dragged) Then
    If Assigned(FOnWordClick) Then
    Begin
      GetWordUnderCursor(X, Y);

      If FWordUnderCursor <> '' Then
        FOnWordClick(Self, FWordUnderCursor, FWordStyleUnderCursor);
    End;

  Inherited MouseUp(Button, Shift, X, Y);
End;

Procedure TCustomMemoEx.MouseMove(Shift: TShiftState; X, Y: integer);
Var
  C                 : TCursor;
  S                 : String;
  i, PY, PX         : integer;
Begin
  MouseMoveX := X;
  MouseMoveY := Y;

  Mouse2Caret(X, Y, MouseMoveXX, MouseMoveYY);

  If X < FGutterWidth Then
    Cursor := crArrow
  Else
    Cursor := crIBeam;

  If (Shift = [ssLeft]) And (mouse_down) Then
  Begin
    mouse_dragged := true;
    Cursor := crIBeam;
    PaintCaret(false);
    If MouseMoveYY <= FLastVisibleRow Then
    Begin
      If Not FCursorBeyondEOL Then
        If YinBounds(MouseMoveYY) Then
        Begin
          If MouseMoveXX > length(FLines.ParaStrings[MouseMoveYY]) Then
            MouseMoveXX := length(FLines.ParaStrings[MouseMoveYY])
              //else
        End
        Else
          MouseMoveXX := 0;

      SetSel(MouseMoveXX, MouseMoveYY);
      SetCaret(MouseMoveXX, MouseMoveYY);
    End;

    TimerScroll.Enabled := (Y < 0) Or (Y > ClientHeight) Or
      (X < 0) Or (X > ClientWidth);

    PaintCaret(true);
  End
  Else If (Assigned(FOnMouseOver)) And (YinBounds(MouseMoveYY)) And (X >=
    FGutterWidth) Then
  Begin
    S := FLines.GetParagraphByIndex(MouseMoveYY, PY, PX);
    GetLineAttr(PY, MouseMoveYY, PX, length(S), 0, MouseMoveXX + 1 + PX, S);
    i := MouseMoveXX + PX + GetAttrDelta(PX, MouseMoveXX + PX,
      FLines.Paragraphs[PY].FAttrs) - 1;
    If i < SelAttrs_Size Then
    Begin
      C := crIBeam;
      FOnMouseOver(Self, FLines.Paragraphs[PY].FAttrs[i].ex_style, C);
      If C <> Cursor Then
        Cursor := C;
    End
    Else
      Cursor := crIBeam;
  End;

  Inherited MouseMove(Shift, X, Y);
End;

Procedure TCustomMemoEx.ScrollTimer(Sender: TObject);

  Procedure ModifyCoordinate(Coordinate1: integer; Var Coordinate2: integer;
    Border, VisibleCount: integer);
  Begin
    If (Coordinate1 < -20) Then
      dec(Coordinate2, VisibleCount)
    Else If (Coordinate1 < 0) Then
      dec(Coordinate2)
    Else If (Coordinate1 > Border + 20) Then
      inc(Coordinate2, VisibleCount)
    Else If (Coordinate1 > Border) Then
      inc(Coordinate2);
  End;

Begin
  If (MouseMoveY < 0) Or (MouseMoveY > ClientHeight) Or (MouseMoveX < 0) Or
    (MouseMoveX > ClientWidth) Then
  Begin
    ModifyCoordinate(MouseMoveY, MouseMoveYY, ClientHeight, FVisibleRowCount);
    //ModifyCoordinate(MouseMoveX, MouseMoveXX, ClientWidth,  FVisibleColCount div 2);   // MHS-
    PaintCaret(false);
    SetSel(MouseMoveXX, MouseMoveYY);
    SetCaret(MouseMoveXX, MouseMoveYY);
    PaintCaret(true);
  End;
End;

Function TCustomMemoEx.GetRealOffs(DefOffs, Index: integer): integer;
Var
  l                 : integer;
Begin
  Result := DefOffs;
  If (Index > -1) And (Index < FLines.FParaLinesCount) Then
  Begin
    l := length(FLines.ParaStrings[Index]);
    If l > 0 Then
      If Result > l Then
        Result := l
      Else
    Else
      Result := 0;
  End;
End;

Function TCustomMemoEx.GetSelText: String;
Var
  sb, se            : integer;
Begin
  Result := '';
  If Not FSelected Then
    exit;
  If Not FSelBlock Then
  Begin
    If (FSelBegY < 0) Or (FSelBegY > FLines.ParaLineCount - 1) Or (FSelEndY < 0)
      Or
      (FSelEndY > FLines.ParaLineCount - 1) Then
    Begin
      Err;
      Exit;
    End;

    // Diese Zeile sorgt dafür, dass die Leerzeichen hinter dem Fixen Text nicht
    // in die Selektion kommen!!!!
    If (FSelBegY = 0) And (FSelBegX < FFixedTextLength) Then
      FSelBegX := FFixedTextLength;     //MHS+

    sb := GetRealOffs(FSelBegX, FSelBegY);
    se := GetRealOffs(FSelEndX, FSelEndY);
    If (se = sb) And (FSelBegY = FSelEndY) Then
      exit;
    sb := PosFromCaret(sb, FSelBegY);
    se := PosFromCaret(se, FSelEndY);
    Result := System.Copy(FLines.Text, sb + 1, se - sb + integer(FInclusive));
  End;
End;

Procedure TCustomMemoEx.SetSelText(Const AValue: String);
Begin
  BeginUpdate;
  Try
    BeginCompound;
    ClearSelection;
    If AValue <> '' Then
    Begin
      InsertText(AValue);
      FSelectedText := true;
      SelStart := PosFromCaret(FSelBegX, FSelBegY) + 1;
      SelLength := Length(AValue);
    End;
    EndCompound;
  Finally
    EndUpdate;
  End;
End;

Procedure TCustomMemoEx.ClipBoardCopy;
Begin
  CopyToClipboard;
End;

Procedure TCustomMemoEx.ReplaceWord(Const NewString: String);
Var
  iBeg, iEnd        : integer;

  Function GetWordOnPos2(S: String; P: integer): String;
  Begin
    Result := '';
    If P < 1 Then
      exit;
    If (S[P] In Separators) And ((P < 1) Or (S[P - 1] In Separators)) Then
      inc(P);
    iBeg := P;
    While iBeg >= 1 Do
      If S[iBeg] In Separators Then
        break
      Else
        dec(iBeg);
    inc(iBeg);
    iEnd := P;
    While iEnd <= Length(S) Do
      If S[iEnd] In Separators Then
        break
      Else
        inc(iEnd);
    If iEnd > iBeg Then
      Result := Copy(S, iBeg, iEnd - iBeg)
    Else
      Result := S[P];
  End;

Var
  S, W              : String;
  X                 : integer;
Begin
  PaintCaret(false);
  BeginUpdate;
  S := FLines.ParaStrings[FCaretY];
  While FCaretX > Length(S) Do
    S := S + ' ';
  W := _Trim(GetWordOnPos2(S, FCaretX));
  If W = '' Then
  Begin
    iBeg := FCaretX + 1;
    iEnd := FCaretX
  End;
  ClearUndo;
  Delete(S, iBeg, iEnd - iBeg);
  Insert(NewString, S, iBeg);
  FLines.InternalParaStrings[FCaretY] := S;
  X := iBeg + Length(NewString) - 1;
  PaintLine(FCaretY, -1, -1);
  SetCaretInternal(X, FCaretY);
  Changed;
  EndUpdate;
  PaintCaret(true);
End;

Procedure TCustomMemoEx.InsertText(Const Text: String);
Var
  S                 : String;
  P                 : integer;
  X, Y              : integer;
Begin
  If Text <> '' Then
  Begin
    PaintCaret(false);
    BeginUpdate;
    Reline;
    S := FLines.Text;
    P := PosFromCaret(FCaretX, FCaretY);

    TInsertUndo.Create(Self, FCaretX, FCaretY, Text);

    Insert(Text, S, P + 1);
    FLines.SetLockText(S);
    CaretFromPos(P + Length(Text), X, Y);
    SetCaretInternal(X, Y);
    Changed;
    EndUpdate;
    PaintCaret(true);
  End;
End;

Procedure TCustomMemoEx.InsertTextAtCurrentPos(Const AText: String);
Var
  S                 : String;
Begin
  BeginUpdate;
  S := ExpandTabs(AdjustLineBreaks(AText));
  If Assigned(FOnTextInsert) Then
    FOnTextInsert(Self, S);
  ClearSelection;
  InsertText(S);
  EndUpdate;
  scbVert.Position := FTopRow;
End;

Procedure TCustomMemoEx.ClipBoardPaste;
Begin
  PasteFromClipboard;
End;

Procedure TCustomMemoEx.ClipBoardCut;
Begin
  CutToClipboard;
End;

Procedure TCustomMemoEx.DeleteSelected;
Begin
  ClearSelection;
End;

Procedure TCustomMemoEx.SetGutterWidth(AWidth: integer);
Begin
  If FGutterWidth <> AWidth Then
  Begin
    FGutterWidth := AWidth;
    UpdateEditorSize;
    Invalidate;
  End;
End;

Procedure TCustomMemoEx.SetGutterColor(AColor: TColor);
Begin
  If FGutterColor <> AColor Then
  Begin
    FGutterColor := AColor;
    FGutter.Invalidate;
  End;
End;

Procedure TCustomMemoEx.SetFont(Value: TFont);
Begin
  FFont.Assign(Value);
  Invalidate;
End;

Procedure TCustomMemoEx.SetLinesOKFontColor(Value: TColor);
Begin
  FLinesOKFontColor := Value;
  Invalidate;
End;

Procedure TCustomMemoEx.SetLinesOK(Value: Integer);
Begin
  FLinesOK := Value;
  FLines.SetText(PChar(Lines.Text));
  Invalidate;
End;

Procedure TCustomMemoEx.SetFixedTextFontColor(Value: TColor);
Begin
  FFixedTextFontColor := Value;
  FLines.SetText(PChar(Lines.Text));
  invalidate;
End;

Procedure TCustomMemoEx.SetFixedText(Value: String);
Begin
  FFixedText := Value;
  FFixedTextLength := Length(FFixedText);
  FLines.SetText(PChar(Lines.Text));
  invalidate;
End;

Function TCustomMemoEx.GetLines: TStrings;
Begin
  Result := FLines;
End;

Procedure TCustomMemoEx.SetLines(ALines: TStrings);
Begin
  If ALines <> Nil Then
    FLines.Assign(ALines);
  ClearUndo;
End;

Procedure TCustomMemoEx.TextAllChanged;
Begin
  TextAllChangedInternal(true);
End;

Procedure TCustomMemoEx.TextAllChangedInternal(Const Unselect: Boolean);
Begin
  If Unselect Then
    FSelectedText := false;
  UpdateEditorSize(false);
  If (Showing) And (FUpdateLock = 0) Then
    Invalidate;
End;

Procedure TCustomMemoEx.SetCols(ACols: integer);
Begin
  If FCols <> ACols Then
  Begin
    FCols := Max(ACols, 1);
    If FCols > FVisibleColCount Then
    Begin
      scbHorz.Max := FCols - 1;
      scbHorz.Min := 0;
    End
    Else
    Begin
      scbHorz.Min := 0;
      scbHorz.Max := 0;
    End;
    scbHorz.LargeChange := FVisibleColCount - 1;
    scbHorz.Page := FVisibleColCount;
  End;
End;

Procedure TCustomMemoEx.SetRows(ARows: integer);
Begin
  If FRows <> ARows Then
  Begin
    FRows := Max(ARows, 1);
    If FRows > FVisibleRowCount Then
    Begin
      scbVert.Max := FRows - 1;
      scbVert.Min := 0;
    End
    Else
    Begin
      scbVert.Min := 0;
      scbVert.Max := 0;
    End;
    scbVert.LargeChange := FVisibleRowCount - 1;
    scbVert.Page := FVisibleRowCount;
  End;
End;

Procedure TCustomMemoEx.SetLeftTop(ALeftCol, ATopRow: integer);
Begin
  If ALeftCol < 0 Then
    ALeftCol := 0;
  If (FLeftCol <> ALeftCol) Then
  Begin
    scbHorz.Position := ALeftCol;
    Scroll(false, ALeftCol);
  End;
  If ATopRow < 0 Then
    ATopRow := 0;
  If (FTopRow <> ATopRow) Then
  Begin
    scbVert.Position := ATopRow;
    Scroll(true, ATopRow);
  End;
End;

Procedure TCustomMemoEx.SetScrollBars(Value: TScrollStyle);
Begin
  If FScrollBars <> Value Then
  Begin
    FScrollBars := Value;
    RecreateWnd;
    UpdateEditorSize;
  End;
End;

Procedure TCustomMemoEx.SetRightMarginVisible(Value: boolean);
Begin
  If FRightMarginVisible <> Value Then
  Begin
    FRightMarginVisible := Value;
    Invalidate;
  End;
End;

Function GetPixelProMM: Double;
Var
  DC                : HDC;
  i, k              : integer;
Begin
  DC := GetDC(GetDesktopWindow);
  Result := GetDeviceCaps(DC, HORZRes) / GetDeviceCaps(DC, HORZSize);
  ReleaseDC(GetDesktopWindow, DC);
End;

Procedure TCustomMemoEx.SetRightMargin(Value: integer);
Begin
  If FRightMargin <> Value Then
  Begin
    If Value < 8 Then
      Raise EInvalidRightMarginValue.Create('Invalid right margin value');
    FRightMargin := Value;
    FRealRightMargin := Value;

    Invalidate2;
  End;
End;

Procedure TCustomMemoEx.SetRightMarginColor(Value: TColor);
Begin
  If FRightMarginColor <> Value Then
  Begin
    FRightMarginColor := Value;
    Invalidate;
  End;
End;

Function TCustomMemoEx.ExpandTabs(Const S: String): String;
Var
  i                 : integer;
  Sp                : String;
Begin
  If Pos(#9, S) > 0 Then
  Begin
    Sp := Spaces(GetDefTabStop(0, true));
    Result := '';
    For i := 1 To Length(S) Do
      If S[i] = #9 Then
        Result := Result + Sp
      Else
        Result := Result + S[i];
  End
  Else
    Result := S;
End;

Procedure TCustomMemoEx.Changed;
Begin
  FModified := true;
  FPEditBuffer := Nil;
  If Assigned(FOnChange) Then
    FOnChange(Self);
  StatusChanged;
End;

Procedure TCustomMemoEx.StatusChanged;
Begin
  If Not ((csDestroying In ComponentState) Or (csLoading In ComponentState))
    Then
    If Assigned(FOnChangeStatus) Then
      FOnChangeStatus(Self);
End;

{
  Convert text offset to the caret coordinates.
}

Procedure TCustomMemoEx.CaretFromPos(Const Pos: integer; Var X, Y: integer);
Var
  i, j, k           : integer;
Begin
  k := 0;
  X := -1;
  Y := -1;
  For i := 0 To FLines.Count - 1 Do
  Begin
    For j := 0 To FLines.Paragraphs[i].FCount - 1 Do
    Begin
      inc(Y);
      inc(k, length(FLines.Paragraphs[i].FStrings[j]));
      If k >= Pos Then
      Begin
        X := Pos - (k - length(FLines.Paragraphs[i].FStrings[j]));
        exit;
      End;
    End;
    inc(k, 2);
  End;
  Y := FLines.ParaLineCount - 1;
  If Y >= 0 Then
    X := length(FLines.ParaStrings[Y]);
End;

{
  Convert caret coordinates to text offset.
}

Function TCustomMemoEx.PosFromCaret(Const X, Y: integer): integer;
Var
  i, j, k, z        : integer;
Begin
  If Y > FLines.ParaLineCount - 1 Then
    Result := length(FLines.Text)
  Else If Y < 0 Then
    Result := -1
  Else
  Begin
    Result := 0;
    k := 0;
    For i := 0 To FLines.Count - 1 Do
      If k + (FLines.Paragraphs[i].FCount - 1) < Y Then
      Begin
        inc(Result, length(FLines[i]) + 2);
        inc(k, FLines.Paragraphs[i].FCount);
      End
      Else
      Begin
        For j := 0 To FLines.Paragraphs[i].FCount - 1 Do
        Begin
          z := length(FLines.Paragraphs[i].FStrings[j]);
          If k + j < Y Then
            inc(Result, z)
          Else
          Begin
            If z > X Then
              z := X;
            inc(Result, z);
            break;
          End;
        End;
        break;
      End;
  End;
End;

Function TCustomMemoEx.PosFromMouse(Const X, Y: integer): integer;
Var
  X1, Y1            : integer;
Begin
  Mouse2Caret(X, Y, X1, Y1);
  If (X1 < 0) Or (Y1 < 0) Then
    Result := -1
  Else
    Result := PosFromCaret(X1, Y1);
End;

Function TCustomMemoEx.GetTextLen: integer;
Begin
  Result := Length(FLines.Text);
End;

Function TCustomMemoEx.GetSelStart: integer;
Begin
  If FSelectedText Then
    Result := PosFromCaret(GetRealOffs(FSelBegX, FSelBegY), FSelBegY) + 1
  Else
    Result := PosFromCaret(GetRealOffs(FCaretX, FCaretY), FCaretY) + 1;
End;

Procedure TCustomMemoEx.SetSelStart(Const ASelStart: integer);
Begin
  FSelectedText := true;
  CaretFromPos(ASelStart - 1, FSelBegX, FSelBegY);
  SetCaretInternal(FSelBegX, FSelBegY);
  SetSelLength(0);
  MakeRowVisible(FSelBegY);
End;

Procedure TCustomMemoEx.MakeRowVisible(ARow: integer);
Begin
  If (ARow < FTopRow) Or (ARow > FLastVisibleRow) Then
  Begin
    ARow := FCaretY - Trunc(VisibleRowCount / 2);
    If ARow < 0 Then
      ARow := 0;
    SetLeftTop(FLeftCol, ARow);
  End;
End;

Function TCustomMemoEx.GetSelLength: integer;
Begin
  Result := Length(GetSelText);
End;

Procedure TCustomMemoEx.SetSelLength(Const ASelLength: integer);
Begin
  FSelectedText := ASelLength > 0;
  CaretFromPos(SelStart + ASelLength - 1, FSelEndX, FSelEndY);
  FUpdateSelBegY := FSelBegY;
  FUpdateSelEndY := FSelEndY;
  SetCaretInternal(FSelEndX, FSelEndY);
  Invalidate;
End;

Procedure TCustomMemoEx.SetLockText(Const Text: String);
Begin
  FLines.SetLockText(Text);
End;

Procedure TCustomMemoEx.GutterPaint(Canvas: TCanvas);
Begin
  Canvas.Font.Assign(FGutter.FFont);
  If Assigned(FOnPaintGutter) Then
    FOnPaintGutter(Self, Canvas);
End;

Procedure TCustomMemoEx.SetMode(index: integer; Value: boolean);
Var
  PB                : ^boolean;
Begin
  Case index Of
    0: PB := @FInsertMode;
  Else                                  {1 :}
    PB := @FReadOnly;
  End;
  If PB^ <> Value Then
  Begin
    PB^ := Value;
    If index = 1 Then
      Invalidate2;
    StatusChanged;
  End;
End;

Function TCustomMemoEx.GetWordOnCaret: String;
Begin
  Result := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX);
End;

Function TCustomMemoEx.GetTabStop(Const X, Y: integer; Const What: TTabStop;
  Const Next: Boolean): integer;

  Procedure UpdateTabStops;
  Var
    S               : String;
    j, i, k         : integer;

    Function ProcessString: boolean;
    Begin
      FLines.CheckLength(S);
      Result := false;
      If (What = tsTabStop) And (length(S) > 0) Then
        FTabPos[length(S) - 1] := true;
      While i <= length(S) Do
      Begin
        If S[i] = ' ' Then
        Begin
          FTabPos[i - 1] := true;
          If (What = tsTabStop) And (i >= X) Then
            Result := true;
        End;
        inc(i);
      End;
    End;

  Begin
    FillChar(FTabPos[0], Max_X, false);
    If (What = tsTabStop) And (FSmartTab) Then
    Begin
      j := 1;
      i := 1;
      While Y - j >= 0 Do
      Begin
        S := _TrimRight(FLines.ParaStrings[Y - j]);
        If ProcessString Then
          break;
        If i >= Max_X Div 4 Then
          break;
        If j >= FVisibleRowCount * 2 Then
          break;
        inc(j);
      End;
    End
    Else If What = tsAutoIndent Then
    Begin
      FLines.Index2ParaIndex(Y, j, i);
      k := 0;
      While j - k >= 0 Do
      Begin
        S := FLines[j - k];
        i := 1;
        If S > '' Then
        Begin
          ProcessString;
          break;
        End;
        If k >= FVisibleRowCount * 2 Then
          break;
        inc(k);
      End;
    End;
  End;

Var
  i                 : integer;
Begin
  UpdateTabStops;
  Result := X;
  If Next Then
  Begin
    For i := X + 1 To High(FTabPos) Do
      If (Not FTabPos[i - 1]) And (What = tsAutoIndent) Then
      Begin
        Result := i - 1;
        exit;
      End
      Else If (Not FTabPos[i]) And (i > 0) Then
        If FTabPos[i - 1] Then
        Begin
          Result := i;
          Exit;
        End;
    If Result = X Then
      Result := GetDefTabStop(X, true);
  End
  Else If Result = X Then
    Result := GetDefTabStop(X, false);
End;

{
  Get default tab stop position depending on current.
}

Function TCustomMemoEx.GetDefTabStop(Const X: integer; Const Next: Boolean):
  integer;
Var
  i                 : integer;
Begin
  i := FTabSize;
  If i = 0 Then
    Result := X
  Else If i > X Then
    Result := i
  Else If X Mod i = 0 Then
    Result := X + i
  Else
    Result := ((X Div i) + 1) * i;
End;

Function TCustomMemoEx.GetBackStop(Const X, Y: integer): integer;

  Procedure UpdateBackStops;
  Var
    S               : String;
    j, i, k         : integer;
  Begin
    j := 1;
    i := X - 1;
    FillChar(FTabPos[0], Max_X, false);
    FTabPos[0] := true;
    While (Y - j >= 0) Do
    Begin
      S := FLines.ParaStrings[Y - j];
      For k := 1 To Min(Length(S), i) Do { Iterate }
        If S[k] <> ' ' Then
        Begin
          i := k;
          FTabPos[i - 1] := true;
          Break;
        End;
      If i = 1 Then
        Break;
      If j >= FVisibleRowCount * 2 Then
        Break;
      inc(j);
    End;
  End;

Var
  i                 : integer;
  S                 : String;
Begin
  Result := X - 1;
  S := _TrimRight(FLines.ParaStrings[Y]);
  If (_Trim(Copy(S, 1, X)) = '') And
    ((X + 1 > Length(S)) Or (S[X + 1] <> ' ')) Then
  Begin
    UpdateBackStops;
    For i := X Downto 0 Do
      If FTabPos[i] Then
      Begin
        Result := i;
        Exit;
      End;
  End;
End;

Procedure TCustomMemoEx.BeginCompound;
Begin
  inc(FCompound);
  TBeginCompoundUndo.Create(Self);
End;

Procedure TCustomMemoEx.EndCompound;
Begin
  TEndCompoundUndo.Create(Self);
  dec(FCompound);
End;

Procedure TCustomMemoEx.BeginRecord;
Begin
  FMacro := '';
  FRecording := true;
  StatusChanged;
End;

Procedure TCustomMemoEx.EndRecord(Var AMacro: TMacro);
Begin
  FRecording := false;
  AMacro := FMacro;
  StatusChanged;
End;

Procedure TCustomMemoEx.PlayMacro(Const AMacro: TMacro);
Var
  i                 : integer;
Begin
  BeginUpdate;
  BeginCompound;
  Try
    i := 1;
    While i < Length(AMacro) Do
    Begin
      Command(byte(AMacro[i]) + byte(AMacro[i + 1]) Shl 8);
      inc(i, 2);
    End;
  Finally
    EndCompound;
    EndUpdate;
  End;
End;

Procedure TCustomMemoEx.CantUndo;
Begin
  FUndoBuffer.Clear;
End;

Procedure TCustomMemoEx.CompletionIdentifier(Var Cancel: boolean);
Begin
  {abstract}
End;

Procedure TCustomMemoEx.CompletionTemplate(Var Cancel: boolean);
Begin
  {abstract}
End;

Procedure TCustomMemoEx.DoCompletionIdentifier(Var Cancel: boolean);
Begin
  CompletionIdentifier(Cancel);
  If Assigned(FOnCompletionIdentifier) Then
    FOnCompletionIdentifier(Self, Cancel);
End;

Procedure TCustomMemoEx.DoCompletionTemplate(Var Cancel: boolean);
Begin
  CompletionTemplate(Cancel);
  If Assigned(FOnCompletionTemplate) Then
    FOnCompletionTemplate(Self, Cancel);
End;

Function TCustomMemoEx.DoPreprocessCompletion(Const ID, OldText: String):
  String;
Begin
  If Assigned(FOnPreprocessCompletion) Then
    Result := FOnPreprocessCompletion(Self, ID, OldText)
  Else
    Result := OldText;
End;

{ TIEditReader support }

Procedure TCustomMemoEx.ValidateEditBuffer;
Begin
  If FPEditBuffer = Nil Then
  Begin
    FEditBuffer := Lines.Text;
    FPEditBuffer := PChar(FEditBuffer);
    FEditBufferSize := Length(FEditBuffer);
  End;
End;

Function TCustomMemoEx.GetText(Position: longint; Buffer: PChar;
  Count: longint): longint;
Begin
  ValidateEditBuffer;
  If Position <= FEditBufferSize Then
  Begin
    Result := Min(FEditBufferSize - Position, Count);
    Move(FPEditBuffer[Position], Buffer[0], Result);
  End
  Else
    Result := 0;
End;

Procedure TCustomMemoEx.SetWordWrap(Value: boolean);
Begin
  If Value <> FWordWrap Then
  Begin
    FWordWrap := Value;
    FLines.Reformat;
    ClearUndo;
  End;
End;

Procedure TCustomMemoEx.SetStripInvisible(Value: boolean);
Begin
  If Value <> FStripInvisible Then
  Begin
    FStripInvisible := Value;
    If FReadOnly Then
      Invalidate2;
  End;
End;

{
}

Procedure TCustomMemoEx.Invalidate2;
Var
  i                 : integer;
Begin
  For i := 0 To FLines.FCount - 1 Do
    FLines.FList^[i].FChanged := true;
  Invalidate;
End;

Procedure TCustomMemoEx.InvalidateGutter;
Begin
  FGutter.Invalidate;
End;

Procedure TCustomMemoEx.InvalidateLine(Index: integer);
Var
  p, pi             : integer;
Begin
  FLines.Index2ParaIndex(Index, p, pi);
  FLines.FList^[p].FChanged := true;
End;

Procedure TCustomMemoEx.FontChanged(Sender: TObject);
Begin
  UpdateEditorSize;
  FLines.SetText(PChar(Lines.Text));
  Invalidate2;
End;

{
  OnAfterLoad event
}

Function TCustomMemoEx.GetAfterLoad: TNotifyEvent;
Begin
  Result := FLines.FOnAfterLoad;
End;

Procedure TCustomMemoEx.SetAfterLoad(Value: TNotifyEvent);
Begin
  FLines.FOnAfterLoad := Value;
End;

{
  OnBeforeSave event
}

Function TCustomMemoEx.GetBeforeSave: TNotifyEvent;
Begin
  Result := FLines.FOnBeforeSave;
End;

Procedure TCustomMemoEx.SetBeforeSave(Value: TNotifyEvent);
Begin
  FLines.FOnBeforeSave := Value;
End;

Procedure TCustomMemoEx.SetSelectedText(Value: boolean);
Begin
  If FSelected <> Value Then
  Begin
    FSelected := Value;
    SelectionChanged;
  End;
End;

Function TCustomMemoEx.GetPlainText: String;
Begin
  Result := StringReplace(FLines.Text, #160, #32, [rfReplaceAll]);
End;

Procedure TCustomMemoEx.SetPlainText(Const AValue: String);
Begin
  If AValue <> FLines.Text Then
    FLines.Text := AValue;
End;

Function TCustomMemoEx.GetTextForPrinter: String;
Var
  i                 : Integer;
  s                 : String;
Begin

  If FLinesOK > 0 Then
  Begin
    For i := 0 To FLinesOK - 1 Do
    Begin
      If i < FLines.FParaLinesCount Then
        s := s + StringReplace(FLines.GetParaString(i), Blank, #32,
          [rfReplaceAll]) + #10#13;
    End;
  End
  Else
  Begin
    For i := 0 To FLines.FParaLinesCount - 1 Do
    Begin
      s := s + StringReplace(FLines.GetParaString(i), Blank, #32, [rfReplaceAll])
        + #10#13;
    End;
  End;

  Result := s;
End;

Function TCustomMemoEx.GetCanUndo: boolean;
Begin
  Result := FUndoBuffer.FPtr >= 0;
End;

Function TCustomMemoEx.GetCanPaste: boolean;
Begin
  Result := Clipboard.HasFormat(CF_TEXT);
End;

Procedure TCustomMemoEx.Clear;
Begin
  FLines.Clear;
  ClearUndo;
End;

Procedure TCustomMemoEx.SetCaretPos(Const AValue: TPoint);
Begin
  SetCaret(AValue.X, AValue.Y);
End;

Procedure TCustomMemoEx.ClearSelection;
Var
  S, S1             : String;
  iBeg, X, Y        : integer;
Begin
  If (FSelected) And (Not FReadOnly) Then
  Begin
    S1 := GetSelText;
    FSelectedText := false;
    If S1 = '' Then
      exit;
    PaintCaret(false);
    S := FLines.Text;
    iBeg := PosFromCaret(FSelBegX, FSelBegY);

    TDeleteSelectedUndo.Create(Self, FCaretX, FCaretY, S1, FSelBlock,
      FSelBegX, FSelBegY, FSelEndX, FSelEndY, iBeg);

    Delete(S, iBeg + 1, length(S1));
    FLines.SetLockText(S);
    CaretFromPos(iBeg, X, Y);
    SetCaretInternal(X, Y);
    Changed;
    UpdateEditorSize(false);
    If FUpdateLock = 0 Then
      Invalidate;
    PaintCaret(true);
  End;
End;

Procedure TCustomMemoEx.ClearUndo;
Begin
  FUndoBuffer.Clear;
End;

Procedure TCustomMemoEx.CopyToClipboard;
Begin
  If Not FSelBlock Then
    _CopyToClipboard(Handle, GetSelText, Font.Charset);
End;

Procedure TCustomMemoEx.CutToClipboard;
Begin
  If Not FReadOnly Then
  Begin
    CopyToClipboard;
    ClearSelection;
  End;
End;

Procedure TCustomMemoEx.PasteFromClipboard;
Var
  ClipS             : String;
Begin
  If Not FReadOnly Then
  Begin
    ClipS := _PasteFromClipboard(Handle, Font.Charset, false);
    InsertTextAtCurrentPos(ClipS);
  End;
End;

Procedure TCustomMemoEx.SelectAll;
Begin
  Command(ecSelAll);
End;

Procedure TCustomMemoEx.Undo;
Begin
  Command(ecUndo);
End;

Function TCustomMemoEx.GetCaretPos: TPoint;
Begin
  Result.X := FCaretX;
  Result.Y := FCaretY;
End;

Function TCustomMemoEx.SearchReplace(Const SearchText, ReplaceText: String;
  Options: TSearchOptions): Integer;
Begin
  Result := 0;
End;

//================================================================================================
//================================================================================================
//================================================================================================
//================================================================================================
//================================================================================================
//================================================================================================

{ TEditKey }

Constructor TEditKey.Create(Const ACommand: TEditCommand; Const AKey1: word;
  Const AShift1: TShiftState);
Begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Command := ACommand;
End;

Constructor TEditKey.Create2(Const ACommand: TEditCommand; Const AKey1: word;
  Const AShift1: TShiftState; Const AKey2: word; Const AShift2: TShiftState);
Begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Key2 := AKey2;
  Shift2 := AShift2;
  Command := ACommand;
End;

{ TKeyboard }

Constructor TKeyboard.Create;
Begin
  List := TList.Create;
End;

Destructor TKeyboard.Destroy;
Begin
  Clear;
  List.Free;
End;

Procedure TKeyboard.Add(Const ACommand: TEditCommand; Const AKey1: word;
  Const AShift1: TShiftState);
Begin
  List.Add(TEditKey.Create(ACommand, AKey1, AShift1));
End;

Procedure TKeyboard.Add2(Const ACommand: TEditCommand; Const AKey1: word;
  Const AShift1: TShiftState; Const AKey2: word; Const AShift2: TShiftState);
Begin
  List.Add(TEditKey.Create2(ACommand, AKey1, AShift1, AKey2, AShift2));
End;

Procedure TKeyboard.Clear;
Var
  i                 : integer;
Begin
  For i := 0 To List.Count - 1 Do
    TObject(List[i]).Free;
  List.Clear;
End;

Function TKeyboard.Command(Const AKey: word; Const AShift: TShiftState):
  TEditCommand;
Var
  i                 : integer;
Begin
  Result := 0;
  For i := 0 To List.Count - 1 Do
    With TEditKey(List[i]) Do
      If (Key1 = AKey) And (Shift1 = AShift) Then
      Begin
        If Key2 = 0 Then
          Result := Command
        Else
          Result := twoKeyCommand;
        Exit;
      End;
End;

Function TKeyboard.Command2(Const AKey1: word; Const AShift1: TShiftState;
  Const AKey2: word; Const AShift2: TShiftState): TEditCommand;
Var
  i                 : integer;
Begin
  Result := 0;
  For i := 0 To List.Count - 1 Do
    With TEditKey(List[i]) Do
      If (Key1 = AKey1) And (Shift1 = AShift1) And
        (Key2 = AKey2) And (Shift2 = AShift2) Then
      Begin
        Result := Command;
        Exit;
      End;
End;

Procedure TKeyboard.SetDefLayout;
Begin
  Clear;
  Add(ecLeft, VK_LEFT, []);
  Add(ecRight, VK_RIGHT, []);
  Add(ecUp, VK_UP, []);
  Add(ecDown, VK_DOWN, []);
  Add(ecSelLeft, VK_LEFT, [ssShift]);
  Add(ecSelRight, VK_RIGHT, [ssShift]);
  Add(ecSelUp, VK_UP, [ssShift]);
  Add(ecSelDown, VK_DOWN, [ssShift]);
  Add(ecBeginLine, VK_HOME, []);
  Add(ecSelBeginLine, VK_HOME, [ssShift]);
  Add(ecBeginDoc, VK_PRIOR, [ssCtrl]);
  Add(ecSelBeginDoc, VK_HOME, [ssCtrl, ssShift]);
  Add(ecEndLine, VK_END, []);
  Add(ecSelEndLine, VK_END, [ssShift]);
  Add(ecEndDoc, VK_NEXT, [ssCtrl]);
  Add(ecSelEndDoc, VK_END, [ssCtrl, ssShift]);
  Add(ecPrevWord, VK_LEFT, [ssCtrl]);
  Add(ecNextWord, VK_RIGHT, [ssCtrl]);
  Add(ecSelPrevWord, VK_LEFT, [ssCtrl, ssShift]);
  Add(ecSelNextWord, VK_RIGHT, [ssCtrl, ssShift]);
  Add(ecSelAll, ord('A'), [ssCtrl, ssShift]);

  Add(ecWindowTop, VK_HOME, [ssCtrl]);
  Add(ecWindowBottom, VK_END, [ssCtrl]);
  Add(ecPrevPage, VK_PRIOR, []);
  Add(ecNextPage, VK_NEXT, []);
  Add(ecSelPrevPage, VK_PRIOR, [ssShift]);
  Add(ecSelNextPage, VK_NEXT, [ssShift]);
  Add(ecScrollLineUp, VK_UP, [ssCtrl]);
  Add(ecScrollLineDown, VK_DOWN, [ssCtrl]);

  Add(ecChangeInsertMode, VK_INSERT, []);

  Add(ecInsertPara, VK_RETURN, []);

  Add(ecBackspace, VK_BACK, []);
  Add(ecBackword, VK_BACK, [ssCtrl]);
  Add(ecDeleteSelected, VK_Back, [ssShift]);
  Add(ecUndo, VK_BACK, [ssAlt]);

  Add(ecDelete, VK_DELETE, []);
  Add(ecTab, VK_TAB, []);
  Add(ecBackTab, VK_TAB, [ssShift]);
  Add(ecDeleteSelected, VK_DELETE, [ssCtrl]);

  Add(ecClipboardCopy, VK_INSERT, [ssCtrl]);
  Add(ecClipboardCut, VK_DELETE, [ssShift]);
  Add(ecClipBoardPaste, VK_INSERT, [ssShift]);

  Add(ecClipboardCopy, ord('C'), [ssCtrl]);
  Add(ecClipboardCut, ord('X'), [ssCtrl]);
  Add(ecClipBoardPaste, ord('V'), [ssCtrl]);

  Add(ecSetBookmark0, ord('0'), [ssCtrl, ssShift]);
  Add(ecSetBookmark1, ord('1'), [ssCtrl, ssShift]);
  Add(ecSetBookmark2, ord('2'), [ssCtrl, ssShift]);
  Add(ecSetBookmark3, ord('3'), [ssCtrl, ssShift]);
  Add(ecSetBookmark4, ord('4'), [ssCtrl, ssShift]);
  Add(ecSetBookmark5, ord('5'), [ssCtrl, ssShift]);
  Add(ecSetBookmark6, ord('6'), [ssCtrl, ssShift]);
  Add(ecSetBookmark7, ord('7'), [ssCtrl, ssShift]);
  Add(ecSetBookmark8, ord('8'), [ssCtrl, ssShift]);
  Add(ecSetBookmark9, ord('9'), [ssCtrl, ssShift]);

  Add(ecGotoBookmark0, ord('0'), [ssCtrl]);
  Add(ecGotoBookmark1, ord('1'), [ssCtrl]);
  Add(ecGotoBookmark2, ord('2'), [ssCtrl]);
  Add(ecGotoBookmark3, ord('3'), [ssCtrl]);
  Add(ecGotoBookmark4, ord('4'), [ssCtrl]);
  Add(ecGotoBookmark5, ord('5'), [ssCtrl]);
  Add(ecGotoBookmark6, ord('6'), [ssCtrl]);
  Add(ecGotoBookmark7, ord('7'), [ssCtrl]);
  Add(ecGotoBookmark8, ord('8'), [ssCtrl]);
  Add(ecGotoBookmark9, ord('9'), [ssCtrl]);

  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), []);
  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), []);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), []);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), []);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), []);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), []);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), []);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), []);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), []);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), []);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), []);
  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), []);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), []);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), []);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), []);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), []);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), []);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), []);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), []);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), []);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecInsertMacro0, ord('S'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecInsertMacro0, ord('S'), [ssCtrl], ord('0'), []);
  Add2(ecInsertMacro1, ord('S'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecInsertMacro1, ord('S'), [ssCtrl], ord('1'), []);
  Add2(ecInsertMacro2, ord('S'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecInsertMacro2, ord('S'), [ssCtrl], ord('2'), []);
  Add2(ecInsertMacro3, ord('S'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecInsertMacro3, ord('S'), [ssCtrl], ord('3'), []);
  Add2(ecInsertMacro4, ord('S'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecInsertMacro4, ord('S'), [ssCtrl], ord('4'), []);
  Add2(ecInsertMacro5, ord('S'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecInsertMacro5, ord('S'), [ssCtrl], ord('5'), []);
  Add2(ecInsertMacro6, ord('S'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecInsertMacro6, ord('S'), [ssCtrl], ord('6'), []);
  Add2(ecInsertMacro7, ord('S'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecInsertMacro7, ord('S'), [ssCtrl], ord('7'), []);
  Add2(ecInsertMacro8, ord('S'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecInsertMacro8, ord('S'), [ssCtrl], ord('8'), []);
  Add2(ecInsertMacro9, ord('S'), [ssCtrl], ord('9'), [ssCtrl]);
  Add2(ecInsertMacro9, ord('S'), [ssCtrl], ord('9'), []);
  Add2(ecInsertMacroA, ord('S'), [ssCtrl], ord('A'), [ssCtrl]);
  Add2(ecInsertMacroA, ord('S'), [ssCtrl], ord('A'), []);
  Add2(ecInsertMacroB, ord('S'), [ssCtrl], ord('B'), [ssCtrl]);
  Add2(ecInsertMacroB, ord('S'), [ssCtrl], ord('B'), []);
  Add2(ecInsertMacroC, ord('S'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecInsertMacroC, ord('S'), [ssCtrl], ord('C'), []);
  Add2(ecInsertMacroD, ord('S'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecInsertMacroD, ord('S'), [ssCtrl], ord('D'), []);
  Add2(ecInsertMacroE, ord('S'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecInsertMacroE, ord('S'), [ssCtrl], ord('E'), []);
  Add2(ecInsertMacroF, ord('S'), [ssCtrl], ord('F'), [ssCtrl]);
  Add2(ecInsertMacroF, ord('S'), [ssCtrl], ord('F'), []);
  Add2(ecInsertMacroG, ord('S'), [ssCtrl], ord('G'), [ssCtrl]);
  Add2(ecInsertMacroG, ord('S'), [ssCtrl], ord('G'), []);
  Add2(ecInsertMacroH, ord('S'), [ssCtrl], ord('H'), [ssCtrl]);
  Add2(ecInsertMacroH, ord('S'), [ssCtrl], ord('H'), []);
  Add2(ecInsertMacroI, ord('S'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecInsertMacroI, ord('S'), [ssCtrl], ord('I'), []);
  Add2(ecInsertMacroJ, ord('S'), [ssCtrl], ord('J'), [ssCtrl]);
  Add2(ecInsertMacroJ, ord('S'), [ssCtrl], ord('J'), []);
  Add2(ecInsertMacroK, ord('S'), [ssCtrl], ord('K'), [ssCtrl]);
  Add2(ecInsertMacroK, ord('S'), [ssCtrl], ord('K'), []);
  Add2(ecInsertMacroL, ord('S'), [ssCtrl], ord('L'), [ssCtrl]);
  Add2(ecInsertMacroL, ord('S'), [ssCtrl], ord('L'), []);
  Add2(ecInsertMacroM, ord('S'), [ssCtrl], ord('M'), [ssCtrl]);
  Add2(ecInsertMacroM, ord('S'), [ssCtrl], ord('M'), []);
  Add2(ecInsertMacroN, ord('S'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecInsertMacroN, ord('S'), [ssCtrl], ord('N'), []);
  Add2(ecInsertMacroO, ord('S'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecInsertMacroO, ord('S'), [ssCtrl], ord('O'), []);
  Add2(ecInsertMacroP, ord('S'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecInsertMacroP, ord('S'), [ssCtrl], ord('P'), []);
  Add2(ecInsertMacroQ, ord('S'), [ssCtrl], ord('Q'), [ssCtrl]);
  Add2(ecInsertMacroQ, ord('S'), [ssCtrl], ord('Q'), []);
  Add2(ecInsertMacroR, ord('S'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecInsertMacroR, ord('S'), [ssCtrl], ord('R'), []);
  Add2(ecInsertMacroS, ord('S'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecInsertMacroS, ord('S'), [ssCtrl], ord('S'), []);
  Add2(ecInsertMacroT, ord('S'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecInsertMacroT, ord('S'), [ssCtrl], ord('T'), []);
  Add2(ecInsertMacroU, ord('S'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecInsertMacroU, ord('S'), [ssCtrl], ord('U'), []);
  Add2(ecInsertMacroV, ord('S'), [ssCtrl], ord('V'), [ssCtrl]);
  Add2(ecInsertMacroV, ord('S'), [ssCtrl], ord('V'), []);
  Add2(ecInsertMacroW, ord('S'), [ssCtrl], ord('W'), [ssCtrl]);
  Add2(ecInsertMacroW, ord('S'), [ssCtrl], ord('W'), []);
  Add2(ecInsertMacroX, ord('S'), [ssCtrl], ord('X'), [ssCtrl]);
  Add2(ecInsertMacroX, ord('S'), [ssCtrl], ord('X'), []);
  Add2(ecInsertMacroY, ord('S'), [ssCtrl], ord('Y'), [ssCtrl]);
  Add2(ecInsertMacroY, ord('S'), [ssCtrl], ord('Y'), []);
  Add2(ecInsertMacroZ, ord('S'), [ssCtrl], ord('Z'), [ssCtrl]);
  Add2(ecInsertMacroZ, ord('S'), [ssCtrl], ord('Z'), []);

  Add(ecUndo, ord('Z'), [ssCtrl]);
  Add(ecUndo, VK_BACK, [ssAlt]);

  Add(ecCompletionIdentifiers, VK_SPACE, [ssCtrl]);
  Add(ecCompletionTemplates, ord('J'), [ssCtrl]);

  { cursor movement - default and classic }
  Add2(ecBeginDoc, ord('Q'), [ssCtrl], ord('R'), []);
  Add2(ecEndDoc, ord('Q'), [ssCtrl], ord('C'), []);

  Add2(ecBeginLine, ord('Q'), [ssCtrl], ord('S'), []);
  Add2(ecEndLine, ord('Q'), [ssCtrl], ord('D'), []);

  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('E'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('X'), []);

  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('T'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('U'), []);

  Add(ecDeleteWord, ord('T'), [ssCtrl]);
  Add(ecInsertPara, ord('N'), [ssCtrl]);
  Add(ecDeleteLine, ord('Y'), [ssCtrl]);

  Add2(ecSelWord, ord('K'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecToUpperCase, ord('K'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecToLowerCase, ord('K'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecChangeCase, ord('O'), [ssCtrl], ord('U'), [ssCtrl]);

  Add2(ecInsertBlock, ord('K'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecSaveBlock, ord('K'), [ssCtrl], ord('W'), [ssCtrl]);

  Add(ecRecordMacro, ord('R'), [ssCtrl, ssShift]);
  Add(ecPlayMacro, ord('P'), [ssCtrl, ssShift]);
  Add(ecSearch, ord('F'), [ssCtrl]);
  Add(ecSearchNext, VK_F3, []);

  Add2(ecBlockOpA, ord('B'), [ssCtrl], ord('A'), [ssCtrl]);
  Add2(ecBlockOpA, ord('B'), [ssCtrl], ord('A'), []);
  Add2(ecBlockOpB, ord('B'), [ssCtrl], ord('B'), [ssCtrl]);
  Add2(ecBlockOpB, ord('B'), [ssCtrl], ord('B'), []);
  Add2(ecBlockOpC, ord('B'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecBlockOpC, ord('B'), [ssCtrl], ord('C'), []);
  Add2(ecBlockOpD, ord('B'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecBlockOpD, ord('B'), [ssCtrl], ord('D'), []);
  Add2(ecBlockOpE, ord('B'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecBlockOpE, ord('B'), [ssCtrl], ord('E'), []);
  Add2(ecBlockOpF, ord('B'), [ssCtrl], ord('F'), [ssCtrl]);
  Add2(ecBlockOpF, ord('B'), [ssCtrl], ord('F'), []);
  Add2(ecBlockOpG, ord('B'), [ssCtrl], ord('G'), [ssCtrl]);
  Add2(ecBlockOpG, ord('B'), [ssCtrl], ord('G'), []);
  Add2(ecBlockOpH, ord('B'), [ssCtrl], ord('H'), [ssCtrl]);
  Add2(ecBlockOpH, ord('B'), [ssCtrl], ord('H'), []);
  Add2(ecBlockOpI, ord('B'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecBlockOpI, ord('B'), [ssCtrl], ord('I'), []);
  Add2(ecBlockOpJ, ord('B'), [ssCtrl], ord('J'), [ssCtrl]);
  Add2(ecBlockOpJ, ord('B'), [ssCtrl], ord('J'), []);
  Add2(ecBlockOpK, ord('B'), [ssCtrl], ord('K'), [ssCtrl]);
  Add2(ecBlockOpK, ord('B'), [ssCtrl], ord('K'), []);
  Add2(ecBlockOpL, ord('B'), [ssCtrl], ord('L'), [ssCtrl]);
  Add2(ecBlockOpL, ord('B'), [ssCtrl], ord('L'), []);
  Add2(ecBlockOpM, ord('B'), [ssCtrl], ord('M'), [ssCtrl]);
  Add2(ecBlockOpM, ord('B'), [ssCtrl], ord('M'), []);
  Add2(ecBlockOpN, ord('B'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecBlockOpN, ord('B'), [ssCtrl], ord('N'), []);
  Add2(ecBlockOpO, ord('B'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecBlockOpO, ord('B'), [ssCtrl], ord('O'), []);
  Add2(ecBlockOpP, ord('B'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecBlockOpP, ord('B'), [ssCtrl], ord('P'), []);
  Add2(ecBlockOpQ, ord('B'), [ssCtrl], ord('Q'), [ssCtrl]);
  Add2(ecBlockOpQ, ord('B'), [ssCtrl], ord('Q'), []);
  Add2(ecBlockOpR, ord('B'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecBlockOpR, ord('B'), [ssCtrl], ord('R'), []);
  Add2(ecBlockOpS, ord('B'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecBlockOpS, ord('B'), [ssCtrl], ord('S'), []);
  Add2(ecBlockOpT, ord('B'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecBlockOpT, ord('B'), [ssCtrl], ord('T'), []);
  Add2(ecBlockOpU, ord('B'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecBlockOpU, ord('B'), [ssCtrl], ord('U'), []);
  Add2(ecBlockOpV, ord('B'), [ssCtrl], ord('V'), [ssCtrl]);
  Add2(ecBlockOpV, ord('B'), [ssCtrl], ord('V'), []);
  Add2(ecBlockOpW, ord('B'), [ssCtrl], ord('W'), [ssCtrl]);
  Add2(ecBlockOpW, ord('B'), [ssCtrl], ord('W'), []);
  Add2(ecBlockOpX, ord('B'), [ssCtrl], ord('X'), [ssCtrl]);
  Add2(ecBlockOpX, ord('B'), [ssCtrl], ord('X'), []);
  Add2(ecBlockOpY, ord('B'), [ssCtrl], ord('Y'), [ssCtrl]);
  Add2(ecBlockOpY, ord('B'), [ssCtrl], ord('Y'), []);
  Add2(ecBlockOpZ, ord('B'), [ssCtrl], ord('Z'), [ssCtrl]);
  Add2(ecBlockOpZ, ord('B'), [ssCtrl], ord('Z'), []);
End;

Procedure RedoNotImplemented;
Begin
  Raise EMemoExError.Create('Redo not yet implemented');
End;

{ TUndoBuffer }

Constructor TUndoBuffer.Create;
Begin
  FCancelUndo := false;
  FPtr := -1;
End;

Procedure TUndoBuffer.Add(Var AUndo: TUndo);
Begin
  If InUndo Then
    exit;
  If FCancelUndo Then
    Clear
  Else
    FMemoEx.StatusChanged;
  While (Count > 0) And (FPtr < Count - 1) Do
  Begin
    TUndo(Items[FPtr + 1]).Free;
    Inherited Delete(FPtr + 1);
  End;
  Inherited Add(AUndo);
  FPtr := Count - 1;
End;

Procedure TUndoBuffer.Undo;
Var
  UndoClass         : TClass;
  Compound          : integer;
Begin
  InUndo := true;
  Try
    If LastUndo <> Nil Then
    Begin
      Compound := 0;
      UndoClass := LastUndo.ClassType;
      While (LastUndo <> Nil) And
        ((UndoClass = LastUndo.ClassType) Or
        (LastUndo Is TDeleteTrailUndo) Or
        (LastUndo Is TReLineUndo) Or
        (Compound > 0)) Do
      Begin
        If LastUndo.ClassType = TBeginCompoundUndo Then
        Begin
          dec(Compound);
          UndoClass := Nil;
        End
        Else If LastUndo.ClassType = TEndCompoundUndo Then
          inc(Compound);
        LastUndo.Undo;
        dec(FPtr);
        If (UndoClass = TDeleteTrailUndo) Or
          (UndoClass = TReLineUndo) Then
          UndoClass := LastUndo.ClassType;
        If Not FMemoEx.FGroupUndo Then
          break;
        // FMemoEx.Paint; {DEBUG !!!!!!!!!}
      End;
      If FMemoEx.FUpdateLock = 0 Then
      Begin
        FMemoEx.TextAllChangedInternal(false);
        FMemoEx.Changed;
      End;
    End;
  Finally
    InUndo := false;
  End;
End;

Procedure TUndoBuffer.Redo;
Begin
  { DEBUG !!!! }
  inc(FPtr);
  LastUndo.Redo;
End;

Procedure TUndoBuffer.Clear;
Begin
  While Count > 0 Do
  Begin
    TUndo(Items[0]).Free;
    Inherited Delete(0);
  End;
  FCancelUndo := false;
  FMemoEx.StatusChanged;
End;

Procedure TUndoBuffer.Delete;
Begin
  If Count > 0 Then
  Begin
    TUndo(Items[Count - 1]).Free;
    Inherited Delete(Count - 1);
    FPtr := Count - 1;
  End;
End;

Function TUndoBuffer.LastUndo: TUndo;
Begin
  If (FPtr >= 0) And (Count > 0) Then
    Result := TUndo(Items[FPtr])
  Else
    Result := Nil;
End;

Function TUndoBuffer.IsNewGroup(Const AUndo: TUndo): boolean;
Begin
  Result := (LastUndo = Nil) Or (LastUndo.ClassType <> AUndo.ClassType)
End;

{ TUndo }

Constructor TUndo.Create(Const AMemoEx: TCustomMemoEx);
Begin
  FMemoEx := AMemoEx;
  UndoBuffer.Add(Self);
End;

Function TUndo.UndoBuffer: TUndoBuffer;
Begin
  If FMemoEx <> Nil Then
    Result := FMemoEx.FUndoBuffer
  Else
    Result := Nil;
End;

{ TCaretUndo }

Constructor TCaretUndo.Create(Const AMemoEx: TCustomMemoEx;
  Const ACaretX, ACaretY: integer);
Begin
  Inherited Create(AMemoEx);
  FCaretX := ACaretX;
  FCaretY := ACaretY;
End;

Procedure TCaretUndo.Undo;
Begin
  With UndoBuffer Do
  Begin
    dec(FPtr);
    While FMemoEx.FGroupUndo And (FPtr >= 0) And Not IsNewGroup(Self) Do
      dec(FPtr);
    inc(FPtr);
    With TCaretUndo(Items[FPtr]) Do
      FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  End;
End;

Procedure TCaretUndo.Redo;
Begin
  RedoNotImplemented;
End;

{ TInsertUndo }

Constructor TInsertUndo.Create(Const AMemoEx: TCustomMemoEx;
  Const ACaretX, ACaretY: integer; Const AText: String);
Var
  i, j              : integer;
Begin
  Inherited Create(AMemoEx, ACaretX, ACaretY);
  FText := AText;
  AMemoEx.FLines.Caret2Paragraph(ACaretX, ACaretY, i, j);
  FOffset := AMemoEx.FLines.GetParaOffs(i) + j;
End;

Procedure TInsertUndo.Undo;
Var
  S, Text           : String;
  iBeg              : integer;
Begin
  Text := '';
  With UndoBuffer Do
  Begin
    While (FPtr >= 0) And Not IsNewGroup(Self) Do
    Begin
      Text := TInsertUndo(LastUndo).FText + Text;
      dec(FPtr);
      If Not FMemoEx.FGroupUndo Then
        break;
    End;
    inc(FPtr);
  End;
  With TInsertUndo(UndoBuffer.Items[UndoBuffer.FPtr]) Do
  Begin
    S := FMemoEx.FLines.Text;
    iBeg := FOffset;
    Delete(S, iBeg + 1, Length(Text));
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  End;
End;

{ TOverwriteUndo }

Constructor TOverwriteUndo.Create(Const AMemoEx: TCustomMemoEx;
  Const ACaretX, ACaretY: integer; Const AOldText, ANewText: String);
Var
  i, j              : integer;
Begin
  Inherited Create(AMemoEx, ACaretX, ACaretY);
  FOldText := AOldText;
  FNewText := ANewText;
  AMemoEx.FLines.Caret2Paragraph(ACaretX, ACaretY, i, j);
  FOffset := AMemoEx.FLines.GetParaOffs(i) + j;
End;

Procedure TOverwriteUndo.Undo;
Var
  S, OldText, NewText: String;
  iBeg              : integer;
Begin
  NewText := '';
  OldText := '';
  With UndoBuffer Do
  Begin
    While (FPtr >= 0) And Not IsNewGroup(Self) Do
    Begin
      OldText := TOverwriteUndo(LastUndo).FOldText + OldText;
      NewText := TOverwriteUndo(LastUndo).FNewText + NewText;
      dec(FPtr);
      If Not FMemoEx.FGroupUndo Then
        break;
    End;
    inc(FPtr);
  End;
  With TOverwriteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) Do
  Begin
    S := FMemoEx.FLines.Text;
    iBeg := FOffset;
    Delete(S, iBeg + 1, Length(NewText));
    Insert(OldText, S, iBeg + 1);
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  End;
End;

{ TDeleteUndo }

Procedure TDeleteUndo.Undo;
Var
  X, Y              : integer;
  S, Text           : String;
  iBeg              : integer;
Begin
  Text := '';
  X := -1;
  Y := -1;
  With UndoBuffer Do
  Begin
    While (FPtr >= 0) And Not IsNewGroup(Self) Do
    Begin
      If (X = -1) Or (Y = -1) Then
      Begin
        X := TDeleteUndo(LastUndo).FCaretX;
        Y := TDeleteUndo(LastUndo).FCaretY;
      End;
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      If Not FMemoEx.FGroupUndo Then
        break;
    End;
    inc(FPtr);
  End;
  If (X <> -1) And (Y <> -1) Then
    With TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) Do
    Begin
      S := FMemoEx.FLines.Text;
      iBeg := FMemoEx.PosFromCaret(X, Y);
      Insert(Text, S, iBeg + 1);
      FMemoEx.FLines.SetLockText(S);
      FMemoEx.CaretFromPos(iBeg, X, Y);
      FMemoEx.SetCaretInternal(X, Y);
    End;
End;

{ TBackspaceUndo }

Procedure TBackspaceUndo.Undo;
Var
  S, Text           : String;
  iBeg              : integer;
  X, Y              : integer;
Begin
  Text := '';
  X := -1;
  Y := -1;
  With UndoBuffer Do
  Begin
    While (FPtr >= 0) And Not IsNewGroup(Self) Do
    Begin
      If (X = -1) Or (Y = -1) Then
      Begin
        X := TDeleteUndo(LastUndo).FCaretX;
        Y := TDeleteUndo(LastUndo).FCaretY;
      End;
      Text := Text + TDeleteUndo(LastUndo).FText;
      dec(FPtr);
      If Not FMemoEx.FGroupUndo Then
        break;
    End;
    inc(FPtr);
  End;
  If (X <> -1) And (Y <> -1) Then
    With TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) Do
    Begin
      S := FMemoEx.FLines.Text;
      iBeg := FMemoEx.PosFromCaret(X, Y);
      Insert(Text, S, iBeg + 1);
      FMemoEx.FLines.SetLockText(S);
      FMemoEx.CaretFromPos(iBeg + length(Text), X, Y);
      FMemoEx.SetCaretInternal(X, Y);
    End;
End;

{ TReplaceUndo }

Constructor TReplaceUndo.Create(Const AMemoEx: TCustomMemoEx; Const ACaretX,
  ACaretY:
  integer; Const ABeg, AEnd: integer; Const AText, ANewText: String);
Begin
  Inherited Create(AMemoEx, ACaretX, ACaretY);
  FBeg := ABeg;
  FEnd := AEnd;
  FText := AText;
  FNewText := ANewText;
End;

Procedure TReplaceUndo.Undo;
Var
  S                 : String;
Begin
  S := FMemoEx.FLines.Text;
  Delete(S, FBeg, Length(FNewText));
  Insert(FText, S, FBeg);
  FMemoEx.FLines.SetLockText(S);
  FMemoEx.SetCaretInternal(FCaretX, FCaretY);
End;

{ TDeleteSelectedUndo }

Constructor TDeleteSelectedUndo.Create(Const AMemoEx: TCustomMemoEx; Const
  ACaretX,
  ACaretY: integer; Const AText: String; Const ASelBlock: boolean; Const
  ASelBegX, ASelBegY,
  ASelEndX, ASelEndY, ASelOffs: integer);
Begin
  Inherited Create(AMemoEx, ACaretX, ACaretY, AText);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
  FSelOffs := ASelOffs;
End;

Procedure TDeleteSelectedUndo.Undo;
Var
  S, Text           : String;
Begin
  Text := '';
  With UndoBuffer Do
  Begin
    While (FPtr >= 0) And Not IsNewGroup(Self) Do
    Begin
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      If Not FMemoEx.FGroupUndo Then
        break;
    End;
    inc(FPtr);
  End;
  With TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) Do
  Begin
    S := FMemoEx.FLines.Text;
    Insert(Text, S, FSelOffs + 1);
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.FSelBlock := FSelBlock;
    FMemoEx.FSelBegX := FSelBegX;
    FMemoEx.FSelBegY := FSelBegY;
    FMemoEx.FSelEndX := FSelEndX;
    FMemoEx.FSelEndY := FSelEndY;
    FMemoEx.FSelectedText := Length(FText) > 0;
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  End;
End;

{ TSelectUndo }

Constructor TSelectUndo.Create(Const AMemoEx: TCustomMemoEx; Const ACaretX,
  ACaretY: integer; Const ASelBlock: boolean; Const ASelBegX, ASelBegY,
  ASelEndX,
  ASelEndY: integer);
Begin
  Inherited Create(AMemoEx, ACaretX, ACaretY);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
End;

Procedure TSelectUndo.Undo;
Begin
  FMemoEx.FSelectedText := (FSelBegX <> FSelEndX) Or (FSelBegY <> FSelEndY);
  FMemoEx.FSelBegX := FSelBegX;
  FMemoEx.FSelBegY := FSelBegY;
  FMemoEx.FSelEndX := FSelEndX;
  FMemoEx.FSelEndY := FSelEndY;
  FMemoEx.FSelBlock := FSelBlock;
  FMemoEx.SetCaretInternal(FCaretX, FCaretY);
End;

Procedure TBeginCompoundUndo.Undo;
Begin
  { nothing }
End;

{ TIndentUndo }

Constructor TIndentUndo.Create(Const AMemoEx: TCustomMemoEx;
  Const AIndentY1, AIndentY2, AIndentSize: integer);
Begin
  FIndentY1 := AIndentY1;
  FIndentY2 := AIndentY2;
  FIndentSize := AIndentSize;
  Inherited Create(AMemoEx);
End;

Procedure TIndentUndo.Undo;
Var
  i                 : integer;
  s                 : String;
Begin
  FMemoEx.FLines.BeginUpdate;
  For i := FIndentY1 To FIndentY2 Do
  Begin
    s := FMemoEx.FLines[i];
    System.Delete(s, 1, FIndentSize);
    FMemoEx.FLines[i] := s;
  End;
  FMemoEx.FLines.EndUpdate;
End;

{ TUnindentUndo }

Constructor TUnindentUndo.Create(Const AMemoEx: TCustomMemoEx; Const AIndentY,
  AIndentSize: integer);
Begin
  FIndentY := AIndentY;
  FIndentSize := AIndentSize;
  Inherited Create(AMemoEx);
End;

Procedure TUnindentUndo.Undo;
Begin
  FMemoEx.FLines[FIndentY] := Spaces(FIndentSize) + FMemoEx.FLines[FIndentY];
End;

Type
  { TMemoExCompletionList }
  TMemoExCompletionList = Class(TListBox)
  Private
    FTimer: TTimer;
    YY: integer;
    // HintWindow : THintWindow;
    Procedure CMHintShow(Var Message: TMessage); Message CM_HINTSHOW;
    Procedure WMCancelMode(Var Message: TMessage); Message WM_CancelMode;
    Procedure OnTimer(Sender: TObject);
  Protected
    Procedure CreateParams(Var Params: TCreateParams); Override;
    Procedure CreateWnd; Override;
    Procedure DestroyWnd; Override;
    Procedure MouseMove(Shift: TShiftState; X, Y: integer); Override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer); Override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      Override;
    Procedure DrawItem(Index: integer; Rect: TRect; State: TOwnerDrawState);
      Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
  End;

  { TCompletion }

Constructor TCompletion.Create2(AMemoEx: TCustomMemoEx);
Begin
  Inherited Create;
  FMemoEx := AMemoEx;
  FPopupList := TMemoExCompletionList.Create(FMemoEx);
  FItemHeight := FPopupList.ItemHeight;
  FDropDownCount := 6;
  FDropDownWidth := 300;
  FTimer := TTimer.Create(Nil);
  FTimer.Enabled := false;
  FTimer.Interval := 800;
  FTimer.OnTimer := OnTimer;
  FIdentifiers := TStringList.Create;
  FTemplates := TStringList.Create;
  FItems := TStringList.Create;
  FAutoChange := TStringList.Create;
  TStringList(FAutoChange).OnChange := AutoChangeChanged;
  FAutoChangeList := TList.Create;
  FDefMode := cmIdentifiers;
  FCaretChar := '|';
  FCRLF := '/n';
  FSeparator := '=';

  FIdentifierColor := AMemoEx.Font.color;
  FReducePopup := True;
End;

Destructor TCompletion.Destroy;
Begin
  Inherited Destroy;
  FPopupList.Free;
  FIdentifiers.Free;
  FTemplates.Free;
  FItems.Free;
  FAutoChange.Free;
  ClearAutoChangeList;
  FAutoChangeList.Free;
  FTimer.Free;
End;

Function TCompletion.GetItems: TStrings;
Begin
  Case FMode Of
    cmIdentifiers: Result := FIdentifiers;
    cmTemplates: Result := FTemplates;
  Else
    Result := Nil;
  End;
End;

Procedure TCompletion.ReplaceWord(Const ANewString: String);
Var
  S, S1, W, NewString: String;
  P, X, Y           : integer;
  iBeg, iEnd        : integer;
  NewCaret, LNum, CX, CY, i: integer;
Begin
  With FMemoEx Do
  Begin
    PaintCaret(false);
    BeginUpdate;
    ReLine;
    S := FLines.Text;
    P := PosFromCaret(FCaretX, FCaretY);
    W := _Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
    LNum := 0;
    If W = '' Then
    Begin
      iBeg := P + 1;
      iEnd := P
    End;
    CaretFromPos(iBeg, CX, CY);
    If CX < 1 Then
      CX := FCaretX + 1;
    NewString := DoPreprocessCompletion(W, ANewString);
    Case FMode Of
      cmIdentifiers:
        Begin
          S1 := NewString;
          NewCaret := Length(NewString);
        End;
      cmTemplates:
        Begin
          S1 := ReplaceWordByPhrase(NewString, FCRLF, #13#10 + Spaces(CX - 1));
          S1 := ReplaceWordByPhrase(S1, FCaretChar, '');
          NewCaret := Pos(FCaretChar, NewString) - 1;
          If NewCaret = -1 Then
            NewCaret := Length(NewString);
          For i := 1 To NewCaret Do
            If S1[i] = #13 Then
              inc(LNum);
        End
    Else
      Raise EMemoExError.Create('Invalid MemoEx Completion Mode');
    End;
    TReplaceUndo.Create(FMemoEx, FCaretX, FCaretY, iBeg, iEnd, W, S1);
    Delete(S, iBeg, iEnd - iBeg);
    Insert(S1, S, iBeg);
    FLines.SetLockText(S);
    CaretFromPos(iBeg - 1 + (CX - 1) * LNum + NewCaret, X, Y);
    SetCaretInternal(X, Y);
    FMemoEx.TextAllChanged;             // Invalidate; {!!!}
    Changed;
    EndUpdate;
    PaintCaret(true);
  End;
End;

Procedure TCompletion.DoKeyPress(Key: Char);
Begin
  If FVisible Then
  Begin
    If HasChar(Key, RAEditorCompletionChars) Then
      SelectItem
    Else
      CloseUp(false)
  End
  Else If FEnabled Then
    FTimer.Enabled := true;
End;

Function TCompletion.DoKeyDown(Key: Word; Shift: TShiftState): boolean;
Begin
  Result := true;
  Case Key Of
    VK_ESCAPE: CloseUp(false);
    VK_RETURN: CloseUp(true);
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      FPopupList.Perform(WM_KEYDOWN, Key, 0);
  Else
    Result := false;
  End;
End;

Procedure TCompletion.DoCompletion(Const AMode: TCompletionList);
Var
  Eq                : boolean;
  Cancel            : boolean;
Begin
  If FMemoEx.FReadOnly Then
    exit;
  If FPopupList.Visible Then
    CloseUp(false);
  FMode := AMode;
  Case FMode Of
    cmIdentifiers: DropDown(AMode, true);
    cmTemplates:
      Begin
        Cancel := false;
        FMemoEx.DoCompletionTemplate(Cancel);
        If Cancel Or (FTemplates.Count = 0) Then
          exit;
        MakeItems;
        FindSelItem(Eq);
        If Eq Then
          ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator))
        Else
          DropDown(AMode, true);
      End;
  End;
End;

Procedure TCompletion.DropDown(Const AMode: TCompletionList; Const ShowAlways:
  boolean);
Var
  ItemCount         : integer;
  P                 : TPoint;
  Y                 : integer;
  PopupWidth, PopupHeight: integer;
  SysBorderWidth, SysBorderHeight: integer;
  R                 : TRect;
  Cancel            : boolean;
  Eq                : boolean;
Begin
  CloseUp(false);
  FMode := AMode;
  With FMemoEx Do
  Begin

    Cancel := false;
    Case FMode Of
      cmIdentifiers: If CaretX = 0 Then
          exit
        Else
          FMemoEx.DoCompletionIdentifier(Cancel);
      cmTemplates: FMemoEx.DoCompletionTemplate(Cancel)
    End;
    MakeItems;
    FindSelItem(Eq);
    If Cancel Or (FItems.Count = 0) Or (((ItemIndex = -1) Or Eq) And Not
      ShowAlways) Then
      exit;

    FPopupList.Items := FItems;
    FPopupList.ItemHeight := FItemHeight;
    FVisible := true;
    SetItemIndex(FItemIndex);
    If FListBoxStyle In [lbStandard] Then
      FPopupList.Style := lbOwnerDrawFixed
    Else
      FPopupList.Style := FListBoxStyle;
    FPopupList.OnMeasureItem := FMemoEx.FOnCompletionMeasureItem;
    FPopupList.OnDrawItem := FMemoEx.FOnCompletionDrawItem;

    ItemCount := FItems.Count;
    SysBorderWidth := GetSystemMetrics(SM_CXBORDER);
    SysBorderHeight := GetSystemMetrics(SM_CYBORDER);
    R := CalcCellRect(FCaretX, FCaretY - FTopRow + 1);
    R.Left := R.Left - FLeftCol * FCellRect.Width; // MHS+

    P := R.TopLeft;
    P.X := ClientOrigin.X + P.X;
    P.Y := ClientOrigin.Y + P.Y;
    Dec(P.X, 2 * SysBorderWidth);
    Dec(P.Y, SysBorderHeight);
    If ItemCount > FDropDownCount Then
      ItemCount := FDropDownCount;
    PopupHeight := ItemHeight * ItemCount + 2;
    Y := P.Y;
    If (Y + PopupHeight) > Screen.Height Then
    Begin
      Y := P.Y - PopupHeight - FCellRect.Height + 1;
      If Y < 0 Then
        Y := P.Y;
    End;
    PopupWidth := FDropDownWidth;
    If PopupWidth = 0 Then
      PopupWidth := Width + 2 * SysBorderWidth;
  End;
  FPopupList.Left := P.X;
  FPopupList.Top := Y;
  FPopupList.Width := PopupWidth;
  FPopupList.Height := PopupHeight;
  SetWindowPos(FPopupList.Handle, HWND_TOP, P.X, Y, 0, 0,
    SWP_NOSIZE Or SWP_NOACTIVATE Or SWP_SHOWWINDOW);
  FPopupList.Visible := true;
End;

{
  Compare of the lines.
  Lines S1 and S2 can be in undefined code page.
}

Function TCompletion.Cmp1(Const S1, S2: String): integer;
Var
  T1, T2            : String;
Begin
  T1 := FMemoEx.DoChangeCase(S1, ME_CASE_CONVERT_LOWER);
  T2 := FMemoEx.DoChangeCase(S2, ME_CASE_CONVERT_LOWER);
  Result := ANSIStrLIComp(PChar(T1), PChar(T2), Length(T2));
End;

{
  Compare of the lines.
  Lines S1 and S2 can be in undefined code page.
}

Function TCompletion.Cmp2(Const S1, S2: String): boolean;
Var
  T1, T2            : String;
Begin
  T1 := FMemoEx.DoChangeCase(S1, ME_CASE_CONVERT_LOWER);
  T2 := FMemoEx.DoChangeCase(S2, ME_CASE_CONVERT_LOWER);
  Result := ANSICompareText(T1, T2) = 0;
End;

Procedure TCompletion.MakeItems;
Var
  i                 : integer;
  S                 : String;
Begin
  Try
    FItems.Clear;
    Case FMode Of
      cmIdentifiers:
        For i := 0 To FIdentifiers.Count - 1 Do
          FItems.Add(FIdentifiers[i]);
      cmTemplates:
        Begin
          With FMemoEx Do
            S := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX);
          For i := 0 To FTemplates.Count - 1 Do
            If Cmp1(FTemplates[i], S) = 0 Then
              FItems.Add(FTemplates[i]);
          If FItems.Count = 0 Then
            FItems.Assign(FTemplates);
        End;
    End;
  Except
  End;
End;

Procedure TCompletion.FindSelItem(Var Eq: boolean);

  Function FindFirst(Ss: TSTrings; S: String): integer;
  Var
    i               : integer;
  Begin

    For i := 0 To Ss.Count - 1 Do
      If Cmp1(Ss[i], S) = 0 Then
      Begin
        Result := i;
        exit;
      End;
    Result := -1;
  End;

Var
  S                 : String;
  i                 : Integer;
  r                 : TRect;
Begin
  Try
    With FMemoEx Do
    Begin
      If CaretX = 0 Then
        exit;
      If FLines.Count > 0 Then
        S := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX)
      Else
        S := '';
    End;

    //============================================================================
    // Reduzierung der Popup-Items
    //============================================================================
    MakeItems;
    i := 0;
    itemIndex := -1;

    If FReducePopup Then
    Begin                               // Popupeinträge nach Eingabe reduzieren
      If (length(Trim(S)) >= 1) Then
      Begin
        While i <= FItems.Count - 1 Do
        Begin
          If lowercase(copy(FItems.Names[i], 1, length(s))) = Lowercase(s) Then
          Begin
            ItemIndex := i;
            inc(i);
          End
          Else
            FItems.Delete(i);
        End;
      End
      Else If length(Trim(S)) = 0 Then
      Begin
        CloseUp(False);
        FItems.Clear;
        Eq := False;
        exit;
      End;

      If (Trim(s) = '') Or
        (itemindex = -1) Or
        (FItems.Count = 0) Then
      Begin
        CloseUp(False);
        FItems.Clear;
        Eq := False;
        exit;
      End
      Else If
        (lowercase(FItems.Names[itemIndex]) = Lowercase(s)) Then
        CloseUp(False)
      Else
      Begin
        FPopupList.Items := FItems;
        FPopupList.ItemIndex := FindFirst(FItems, trim(s));

        If FItems.Count < FDropDownCount Then
          FPopupList.Height := ItemHeight * FItems.Count + 2
        Else
          FPopupList.Height := ItemHeight * FDropDownCount + 2;
      End;

    End
    Else
    Begin
      itemindex := FindFirst(FItems, trim(s));

      If (Itemindex = -1) Or (trim(s) = '') Then
      Begin

        CloseUp(False);
        FItems.Clear;
        Eq := False;
        exit;
      End;
    End;

    //============================================================================

    Eq := (ItemIndex > -1) And Cmp2(_Trim(SubStr(FItems[ItemIndex], 0,
      FSeparator)), S)
  Except
  End;
End;

Procedure TCompletion.SelectItem;
Var
  Cancel            : boolean;
  Param             : boolean;
Begin
  FindSelItem(Param);
  Cancel := Not Visible And (ItemIndex = -1);
  Case FMode Of
    cmIdentifiers: FMemoEx.DoCompletionIdentifier(Cancel);
    cmTemplates: FMemoEx.DoCompletionTemplate(Cancel);
  End;
  If Cancel Or (GetItems.Count = 0) Then
    CloseUp(false);
End;

Procedure TCompletion.CloseUp(Const Apply: boolean);
Begin
  FItemIndex := ItemIndex;
  FPopupList.Visible := false;
  FVisible := false;
  FTimer.Enabled := false;
  If Apply And (ItemIndex > -1) Then
    Case FMode Of
      cmIdentifiers: ReplaceWord(SubStr(FItems[ItemIndex], 0, FSeparator));
      cmTemplates: ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator));
    End;
End;

Procedure TCompletion.OnTimer(Sender: TObject);
Begin
  DropDown(FDefMode, false);
End;

Procedure TCompletion.ClearAutoChangeList;
Var
  i                 : integer;
Begin
  For i := 0 To FAutoChangeList.Count - 1 Do
    Dispose(FAutoChangeList[i]);
  FAutoChangeList.Clear;
End;

Procedure TCompletion.UpdateAutoChange;
Begin
  AutoChangeChanged(FAutoChange);
End;

Procedure TCompletion.AutoChangeChanged(Sender: TObject);

  Procedure AddAutoChangeWord(Const OldWord, NewWord: String);
  Var
    ACW             : PAutoChangeWord;
  Begin
    If OldWord <> '' Then
    Begin
      New(ACW);
      ACW.OldWord := FMemoEx.DoChangeCase(OldWord, ME_CASE_CONVERT_LOWER);
      ACW.NewWord := NewWord;
      FAutoChangeList.Add(ACW);
    End;
  End;

Var
  i                 : integer;
Begin
  ClearAutoChangeList;
  For i := 0 To FAutoChange.Count - 1 Do
    AddAutoChangeWord(SubStr(FAutoChange.Strings[i], 0, FSeparator),
      SubStr(FAutoChange.Strings[i], 1, FSeparator));
  FAutoChangeList.Sort(AutoChangeCompare);
End;

Procedure TCompletion.SetStrings(index: integer; AValue: TStrings);
Begin
  Case index Of
    0: FIdentifiers.Assign(AValue);
    1: FTemplates.Assign(AValue);
    2: FAutoChange.Assign(AValue);
  End;
End;

Function TCompletion.GetItemIndex: integer;
Begin
  Result := FItemIndex;
  If FVisible Then
    Result := FPopupList.ItemIndex;
End;

Procedure TCompletion.SetItemIndex(AValue: integer);
Begin
  FItemIndex := AValue;
  If FVisible Then
    FPopupList.ItemIndex := FItemIndex;
End;

Function TCompletion.GetInterval: cardinal;
Begin
  Result := FTimer.Interval;
End;

Procedure TCompletion.SetInterval(AValue: cardinal);
Begin
  FTimer.Interval := AValue;
End;

{ TMemoExCompletionList }

Constructor TMemoExCompletionList.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Left := -1000;
  Visible := false;
  TabStop := false;
  ParentFont := false;
  Parent := Owner As TCustomMemoEx;
  Ctl3D := false;
  FTimer := TTimer.Create(Nil);
  FTimer.Enabled := false;
  FTimer.Interval := 200;
  FTimer.OnTimer := OnTimer;
  Style := lbOwnerDrawFixed;
  ItemHeight := 13;
End;

Destructor TMemoExCompletionList.Destroy;
Begin
  FTimer.Free;
  Inherited Destroy;
End;

Procedure TMemoExCompletionList.CreateParams(Var Params: TCreateParams);
Begin
  Inherited CreateParams(Params);
  With Params Do
  Begin
    Style := Style Or WS_BORDER;
    ExStyle := ExStyle Or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style Or CS_SAVEBITS;
  End;
End;

Procedure TMemoExCompletionList.CreateWnd;
Begin
  Inherited CreateWnd;
  If Not (csDesigning In ComponentState) Then
    Windows.SetParent(Handle, 0);
End;

Procedure TMemoExCompletionList.DestroyWnd;
Begin
  Inherited DestroyWnd;
End;

Procedure TMemoExCompletionList.MouseMove(Shift: TShiftState; X, Y: integer);
Var
  F                 : integer;
Begin
  YY := Y;
  If KeyPressed(VK_LBUTTON) Then
  Begin
    F := ItemAtPos(Point(X, Y), true);
    If F > -1 Then
      ItemIndex := F;
    FTimer.Enabled := (Y < 0) Or (Y > ClientHeight);
    If (Y < -ItemHeight) Or (Y > ClientHeight + ItemHeight) Then
      FTimer.Interval := 50
    Else
      FTimer.Interval := 200;
  End;
End;

Procedure TMemoExCompletionList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: integer);
Var
  F                 : integer;
Begin
  MouseCapture := true;
  F := ItemAtPos(Point(X, Y), true);
  If F > -1 Then
    ItemIndex := F;
End;

Procedure TMemoExCompletionList.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: integer);
Begin
  MouseCapture := false;
  (Owner As TCustomMemoEx).FCompletion.CloseUp(
    (Button = mbLeft) And PtInRect(ClientRect, Point(X, Y)));
End;

Procedure TMemoExCompletionList.OnTimer(Sender: TObject);
Begin
  If (YY < 0) Then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  Else If (YY > ClientHeight) Then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0);
End;

Procedure TMemoExCompletionList.WMCancelMode(Var Message: TMessage);
Begin
  (Owner As TCustomMemoEx).FCompletion.CloseUp(false);
End;

Procedure TMemoExCompletionList.CMHintShow(Var Message: TMessage);
Begin
  Message.Result := 1;
End;

Procedure TMemoExCompletionList.DrawItem(Index: integer; Rect: TRect; State:
  TOwnerDrawState);
Var
  Offset, W         : integer;
  S                 : String;
Begin
  If Assigned(OnDrawItem) Then
    OnDrawItem(Self, Index, Rect, State)
  Else
  Begin
    Canvas.FillRect(Rect);
    Offset := 3;

    With (Owner As TCustomMemoEx).FCompletion Do
      Case FMode Of
        cmIdentifiers:
          Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
            Separator));
        cmTemplates:
          Begin
            Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
              Separator));
            Canvas.Font.Style := [fsBold];
            S := SubStr(Items[Index], 0, Separator);
            W := Canvas.TextWidth(S);
            Canvas.TextOut(Rect.Right - 2 * Offset - W, Rect.Top, S);
          End;
      End;
  End;
End;

Procedure Register;
Begin
  RegisterComponents('MHS', [TMemoEx]);
End;

procedure TCustomMemoEx.SetTopRow(const Value: integer);
begin
  FTopRow := Value;
end;

End.


