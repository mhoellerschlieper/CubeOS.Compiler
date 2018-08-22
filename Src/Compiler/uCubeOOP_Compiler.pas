{
'*******************************************************************************
' Projekt : Cube-OOP
' Programm: CubeOOP.exe
' Modul   : uCubeOOP_Compiler
' File    : uCubeOOP_Compiler.Pas
' Autor   : M. Höller-Schlieper
' Datum   :
' Sprache : Borland Delphi 7
' Platform: Windows Win7 deutsch SP4
'
' Historie:
'           29.07.2007  MH Erstellung

Compiler Infos:

Registerbenutzung:
    ESP - Stackpointer
    ESI - Objektpointer (Instanzzeiger) einer Membervariablen oder Array
    EBP - Basepointer für lokale Variablen

    EAX - Rechnen
    EDX - For-Schleifen/Rechnen
    ECX - Feldzeiger

Verwaltung der Variablen:
 lade den Offset der Variablen

 mov EAX,[Var]  , wenn globale Variable
 [ESI + Offset] , wenn Member einer Klasse mit ESI als Instanzzeiger
 [EBP - Offset] , wenn Parameter in Funktion
 [EBP + Offset] , wenn lokale Variable

'*******************************************************************************
}

Unit uCubeOOP_Compiler;

Interface

Uses
  SysUtils,
  Classes,
  MAth,
  uCubeOOP_Globals,
  uCubeOOP_Parser;

Type
  TCompiler = Class
  private
    sFilename: String;
    sASMName: String;
    fAsm: TextFile;
    oParser: TCubeOOPInterpreterParser;
    oVarList: TVarList;
    oStrings: TStringList;

    tkToken: TTokenKind;
    sToken: String;
    bError: Boolean;
    sClassName: String;
    iLabelCounter: Integer;

    Procedure OnNewLine(sActualLine: String);
    Procedure EmptyDescriptor(Var Descriptor: TDescriptor);
    Procedure Init(sFilename, sASMName: String);
    Procedure Error(sErrorText: String);
    Procedure ParseError(TokenKind: TTokenKind);

    Procedure wAsm(sASM: String);
    Function GetStringFromFilename(sFilename: String): String;
    Function NextToken: TTokenKind;

    Function Pass1: Boolean;

    // lade den Variablen Offset nach ax
    Function LoadVarOffset(cVarEnter: TcVarEnter; Var sAdress: String; Var
      iOffset: Integer): Boolean;

    // laden den aktuellen Variablenwert nach ax
    Function LoadVarValue(cVarEnter: TcVarEnter): Boolean;

    Function CastType(Type1: TType; Type2: TType): TType;

    Procedure Call_Prolog;
    Procedure Call_Epilog;

    Procedure Call_Class;
    Procedure Call_Const;
    Procedure Call_Var;
    Function Call_Ident(Var sAdress: String; sClass: String): tType;
    Procedure Call_Deklaration(Descriptor: TDescriptor);
    Function Call_DataType(Descriptor: TDescriptor): TType;
    Procedure Call_BringParamsOnStack(MyVar: TcVarEnter);

    Function Call_Term: tType;
    Function Call_Factor: tType;
    Function Call_Expression: tType;

    Procedure Call_Condition;
    Procedure Call_Assignment;

    Procedure Call_Write;
    Procedure Call_Read;
    Procedure Call_If;
    Procedure Call_While;
    Procedure Call_Repeat;
    Procedure Call_Procedure(Descriptor: TDescriptor);
    Procedure Call_Function(Descriptor: TDescriptor);
    Procedure Call_For;
    Procedure Call_Begin;
    Procedure Call_Main;
    Procedure Call_Implementation(Descriptor: TDescriptor);
    Procedure Call_ProcedureParameters(cVarEnter: TcVarEnter);

    Procedure Call_Statement;

  public
    InfoStruct: TInfoStruct;
  End;

Function Call_Compiler(
  sFilename: String;
  sASMName: String;
  Var InfoStruct: TInfoStruct): Boolean;

Var
  Compiler          : TCompiler;

Implementation

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function Call_Compiler(
  sFilename: String;
  sASMName: String;
  Var InfoStruct: TInfoStruct): Boolean;

Begin
  If Compiler = Nil Then
    Compiler := TCompiler.Create;

  Compiler.Init(sFilename, sASMName);
  Result := Compiler.Pass1;
  InfoStruct := Compiler.InfoStruct;
  InfoStruct.VarList := Compiler.oVarList;
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.GetStringFromFilename(sFilename: String): String;
Var
  F                 : TextFile;
  sLine             : String;
Begin
  Result := '';
  If FileExists(sFilename) Then
  Begin
    Try
      AssignFile(F, sFilename);
      Reset(F);
      While Not EOF(F) Do
      Begin
        ReadLn(F, sLine);
        Result := Result + sLine + #13;
      End;
    Finally
      Close(F);
    End;
  End;
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Init(sFilename, sASMName: String);
Begin
  Decimalseparator := '.';
  Self.sFilename := sFilename;
  Self.sASMName := sASMName;

  bError := False;
  iLabelCounter := 1;

  oParser := TCubeOOPInterpreterParser.Create;
  oParser.Source := GetStringFromFilename(sFilename);
  oParser.OnNewLine := OnNewLine;
  oVarList := TVarList.Create;

  oStrings := TStringList.Create;

  AssignFile(fAsm, sAsmNAme);
  Rewrite(fAsm);
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Error(sErrorText: String);
Begin
  bError := True;
  If InfoStruct.sErrorText <> '' Then
  Begin
    InfoStruct.sErrorText := InfoStruct.sErrorText + #13;
  End;

  InfoStruct.sErrorText :=
    InfoStruct.sErrorText +
    'Fehler in Zeile: ' + Inttostr(oParser.iLineNumber + 1) + ' (' + sToken +
    ') ' +
    sErrorText;

  InfoStruct.iErrorLine := oParser.iLineNumber + 1;
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.ParseError(TokenKind: TTokenKind);
Begin
  Error('Incorrect Syntax: Missing - "' + inttostr(TokenKind) + '"');
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.NextToken: TTokenKind;
Begin
  tkToken := oParser.NextToken(sToken);
  Result := tkToken;
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.wAsm(sASM: String);
Begin
  Writeln(fAsm, Trim(sAsm));
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.Pass1: Boolean;
Begin
  Try
    // Header und Spung ins Main
    Call_Prolog;

    // die eigentliche Kompilation
    Repeat
      NextToken;
      Call_Statement;
    Until bError Or (sToken = '');

    // globale Variablen, Konstanten etc.
    Call_Epilog;
    Result := Not bError;
  Finally
    // Assembkler schließen
    CloseFile(fAsm);
  End;
End;

{*******************************************************************************
Procedure:  TCompiler.
Description: bei Aufruf einer Prozedur werden die Parameter auf einen Stack
             gelegt
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_BringParamsOnStack(MyVar: TcVarEnter);
Begin

End;

{*******************************************************************************
Procedure:  TCompiler.Call_Factor
Description: Unterste Ebene (Blatt) im Parserbaum. Hier werden die Basis - Typen
            aufgelöst.
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.Call_Factor: tType;
Var
  iSize             : Integer;
  iOffset           : Integer;
  sAdress           : String;
  MyVar             : TcVArEnter;
  ExpressionType    : TType;
  sW32              : String;
  sChar             : String;
Begin
  Case tkToken Of
    ttLS:
      Begin                             // [
        NextToken;
        ExpressionType := Call_Expression;
        If tkToken = ttRS Then          // ]
        Begin
          NextToken;
        End
        Else
          ParseError(ttRB);

        Case ExpressionType Of
          tInt32, tInt16: Result := ExpressionType;
        Else
          Self.Error('Type Error in [..]');
        End;

      End;
    ttNIL:
      Begin
        NextToken;
        wASM('  Mov EAX, 0');
        Result := tClass;
      End;
    ttGetAllocMem:
      Begin
        NextToken;
        wASM('  Mov EAX, Allocated');
        Result := tInt32;
      End;
    ttTrue:                             // Konstante: TRUE
      Begin
        NextToken;
        wASM('  mov ax,1');
        Result := tBoolean;
      End;
    TTFalse:                            // Konstante: False
      Begin
        NextToken;
        wASM('  mov ax,0');
        Result := tBoolean;
      End;
    ttClass:                            // lade die Größe des Objekts
      Begin
        iSize := oVarList.FindClass(sToken).iOffset;
        wAsm('mov EAX,' + inttostr(iSize));
        Result := tInt32;
      End;
    ttCreate:                           // Create(Klassenname)
      Begin
        If NextToken = ttLB Then        // (
        Begin
          NextToken;
          Call_Expression;

          wAsm('CALL $_Allocate');

          If tkToken = ttRB Then        // )
          Begin
            NextToken;
          End
          Else
            ParseError(ttRB);
        End
        Else
          ParseError(ttLB);
        REsult := tClass;
      End;
    ttBoolean:                          // Boolean - Wert
      Begin
        wASM('  mov al,' + sToken);
        NextToken;
        Result := tBoolean;
      End;
    ttChar:                             // Char - Wert
      Begin
        wASM('  mov al,' + sToken);
        NextToken;
        Result := tChar;
      End;
    ttSingle:                           //Float- Wert
      Begin
        sW32 := SingleToHex(StrToFloat(sToken));
        wASM('  mov eax,' + sW32);
        NextToken;
        REsult := tSingle;
      End;
    ttInt16:                            // Integer - Wert
      Begin
        wASM('  mov eax,' + sToken);
        NextToken;

        Result := tInt16;
      End;
    ttInt32:                            // Integer - Wert
      Begin
        wASM('  mov eax,' + sToken);
        NextToken;

        Result := tInt32;
      End;
    ttString:                           // "konstanter String"  oder char
      Begin

        If Length(sToken) = 3 Then      // dann Char (mit Anführungszeichen!!!)
        Begin
          sChar := Copy(sToken, 2, 1);
          wASM('  XOR eax,eax');
          wASM('  mov al,' + inttostr(ord(sChar[1])));
          NextToken;
          Result := tchar;
        End
        Else
        Begin                           // sonst String
          oStrings.Add(sToken);
          wASM('  LEA eax, $_S' + InttoStr(oStrings.count)); // lade den konstanten String
          NextToken;
          Result := tString;
        End;
      End;
    ttDoubleQuote:                      // "konstanter String"
      Begin
        NextToken;
        sToken := sToken;
        Result := tString;
      End;
    ttIdentifier:                       // Bezeichner
      Begin
        MyVar := oVarLIst.FindClassIdent(sToken, sClassName); //Find(sToken);

        If MyVar = Nil Then
        Begin
          // Sonst suche ob wir eine Klasse sind
          If oVarList.FindClass(sToken) <> Nil Then
          Begin
            //... wenn ja, dann lade die Größe des Objektes
            iSize := oVarList.FindClass(sToken).iOffset;
            wAsm('mov EAX,' + inttostr(iSize));
            Result := tInt32;
            NextToken;
          End
          Else
          Begin
            Error('Variable nicht gefunden: ' + sToken);
            Result := tNone;
          End;
        End
        Else
        Begin

          Case MyVar.tpType Of
            tFunction:                  // sollte der Ident ein Func-call sein....
              Begin
                LoadVarOffset(MyVar, sAdress, iOffset);
                wASM('  mov eax, ' + sAdress);
                NextToken;
                Call_ProcedureParameters(MyVar); // Parameter auf den Stack legen
                wASM('call ' + MyVar.sClass + '_' + MyVar.sName); // Proc aufrufen
                NextToken;
                Result := MyVar.Resulttype; //tFunction;
              End;
            tProcedure:                 // sollte der Ident ein Proc-call sein....
              Begin
                LoadVarOffset(MyVar, sAdress, iOffset);
                wASM('  mov eax, ' + sAdress);
                NextToken;
                Call_ProcedureParameters(MyVar); // Parameter auf den Stack legen
                wASM('call ' + MyVar.sClass + '_' + MyVar.sName); // Proc aufrufen
                NextToken;

                Result := tProcedure;
              End;
            tClass:                     // Objekt auflösen
              Begin
                Result := Call_Ident(sAdress, MyVar.sClass); // IDENT MACHT NEXTTOKEN

                If sAdress <> '' Then
                  wASM('  mov eax,' + sAdress);
              End;
            tInt16:                     // Integer
              Begin
                Result := Call_Ident(sAdress, MyVar.sClass); // IDENT MACHT NEXTTOKEN
                wASM('  mov ax, ' + sAdress);
              End;
            tInt32:                     // Integer
              Begin
                Result := Call_Ident(sAdress, MyVar.sClass); // IDENT MACHT NEXTTOKEN
                wASM('  mov eax,' + sAdress);
              End;
            tSingle:                    // Integer
              Begin
                Result := Call_Ident(sAdress, MyVar.sClass); // IDENT MACHT NEXTTOKEN
                wASM('  mov eax,' + sAdress);
              End;

            tString:                    // Integer
              Begin
                Result := Call_Ident(sAdress, MyVar.sClass); // IDENT MACHT NEXTTOKEN
                wASM('  mov eax, dword ptr ' + sAdress);
              End;
            tArray:
              Begin
                Result := Call_Ident(sAdress, MyVar.sClass); // IDENT MACHT NEXTTOKEN
                wASM('  mov eax,dword ptr ' + sAdress);
              End;
            tChar:                      // Integer
              Begin
                Result := Call_Ident(sAdress, MyVar.sClass); // IDENT MACHT NEXTTOKEN
                wASM('  mov al,' + sAdress);
              End;

          Else
            Begin
              Result := MyVar.tpType;
              NextToken;
            End;
          End;
        End;
      End;
  End;                                  // OF CASE
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.Call_Term: TType;
Var
  tkSaveToken       : Integer;
  Loc_Typ1, Loc_Typ2, Result_TypeLeft, Result_TypeRight: TType;
Begin

  Loc_Typ1 := Call_Factor;
  Result_TypeLeft := Loc_Typ1;

  While Not bError And (tkToken In [ttIntDiv, ttMod, ttMul, ttDiv]) Do
  Begin
    Case Result_TypeLeft Of
      tInt16: wASM('push ax');
      tInt32: wASM('push eax');
      tClass: wASM('push eax');
      tSingle: wASM('push eax');
    Else
      wASM('push eax');
    End;

    tkSaveToken := tkToken;
    NextToken;

    Loc_Typ2 := Call_Factor;

    Result_TypeRight := CastType(Loc_Typ1, Loc_Typ2);

    If Result_TypeRight = tNone Then
      Error('Typenfehler');

    Case Result_TypeLeft Of             // Achtung: lade den Left-Typ!!!
      tInt16: wASM('pop dx');
      tInt32: wASM('pop edx');
      tClass: wASM('pop edx');
      tSingle: wASM('pop edx');
    Else
      wASM('pop edx');
    End;

    If tkSaveToken = ttMul Then
    Begin
      Case Result_TypeRight Of
        tInt16: wASM('  mul dx');
        tInt32: wASM('  mul edx');
        tSingle:
          Begin
            // Zu testen: ESP+4 oder ESP-4 ???????????????????????????????????
            wASM('  SUB  ESP,8');
            wASM('  MOV  [ESP], EDX');  // -4
            wASM('  MOV  [ESP+4], EAX'); // -8
            wASM('  FLD  dword ptr[ESP]');
            wASM('  FMUL dword ptr[ESP+4]');
            wASM('  FSTP dword ptr[ESP+4]');
            wASM('  WAIT');
            wASM('  MOV EAX, dword ptr[ESP+4]');
            wASM('  ADD ESP,8');
          End;
      End;
    End
    Else If tkSaveToken = ttDiv Then
    Begin
      wASM('  xchg ax,dx');
      wASM('  mov bx,dx');
      wASM('  mov cx,0');
      wASM('  mov dx,0');
      wASM('  call ddiv');
    End
    Else If tkSaveToken = ttIntDiv Then
    Begin
      wASM('  xchg ax,dx');
      wASM('  mov bx,dx');
      wASM('  mov cx,0');
      wASM('  mov dx,0');
      wASM('  call ddiv');
    End
    Else If tkSaveToken = ttMod Then
    Begin
      wASM('  xchg ax,dx');
      wASM('  mov bx,dx');
      wASM('  mov cx,0');
      wASM('  mov dx,0');
      wASM('  call dmod');
      wASM('  mov ax,bx');

    End;
  End;

  Result := Result_TypeLeft;
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.Call_Expression: TType;
Var
  tkSaveToken       : Integer;
  bMinus            : Boolean;
  Loc_Typ1, Loc_Typ2, Result_Type: TType;
Begin
  bMinus := false;
  //If tkToken = tInteger Then
  If tkToken In [ttPlus, ttMinus] Then
  Begin
    If tkToken = ttMinus Then
      bMinus := true;
    NextToken;
  End;

  Loc_Typ1 := Call_Term;
  Result_Type := Loc_Typ1;

  If bMinus Then
  Begin
    Case Result_Type Of
      tInt16:
        Begin
          wASM('  mov EDX,-1');
          wASM('  mul EDX');
        End;
      tInt32:
        Begin
          wASM('  mov EDX,-1');
          wASM('  mul EDX');
        End;
      tSingle:
        Begin
        End;
    End;
  End;

  //
  While Not bError And (tkToken In [ttPlus, ttMinus]) Do
  Begin
    Case Result_Type Of
      tBoolean: wASM(' push al');
      tByte: wASM(' push al');
      tchar: wASM(' push al');
      tInt16: wASM(' push ax');
      tInt32: wASM(' push eax');
      tSingle: wASM(' push eax');
      tClass: wASM(' push eax');
      tString: wASM(' push eax');
    End;

    tkSaveToken := tkToken;
    NextToken;

    Loc_Typ2 := Call_Term;

    // wie kann der Typ gecastet werden....
    Result_Type := CastType(Loc_Typ1, Loc_Typ2);

    If Result_Type = TNone Then
      Error('Typenfehler');

    Case Result_Type Of
      tBoolean: wASM(' pop dl');
      tByte: wASM(' pop dl');
      tchar: wASM(' pop dl');
      tInt16: wASM(' pop DX');
      tInt32: wASM(' pop EDX');
      tSingle: wASM(' pop EDX');
      tClass: wASM(' pop EDX');
      tString: wASM(' pop EDX');
    End;

    If tkSaveToken = ttPlus Then
    Begin
      Case Result_Type Of
        tInt16: wASM('  add dx,ax');
        tInt32: wASM('  add edx,eax');
        tSingle:
          Begin
            // Zu testen: ESP+4 oder ESP-4 ???????????????????????????????????
            wASM('  SUB  ESP,8');
            wASM('  MOV  [ESP], EDX');  // -4
            wASM('  MOV  [ESP+4], EAX'); // -8
            wASM('  FLD  dword ptr[ESP]');
            wASM('  FADD dword ptr[ESP+4]');
            wASM('  FSTP dword ptr[ESP+4]');
            wASM('  WAIT');
            wASM('  MOV EDX, dword ptr[ESP+4]');
            wASM('  ADD ESP,8');
          End;
        tString: ;                      // String - CONCAT
      End;
    End
    Else
    Begin
      If tkSaveToken = ttMinus Then
      Begin
        Case Result_Type Of
          tInt16: wASM('  sub dx,ax');
          tInt32: wASM('  sub edx,eax');
          tSingle:
            Begin
              // Zu testen: ESP+4 oder ESP-4 ???????????????????????????????????
              wASM('  SUB  ESP,8');
              wASM('  MOV  [ESP], EDX'); // -4
              wASM('  MOV  [ESP+4], EAX'); // -8
              wASM('  FLD  dword ptr[ESP]');
              wASM('  FSUB dword ptr[ESP+4]');
              wASM('  FSTP dword ptr[ESP+4]');
              wASM('  WAIT');
              wASM('  MOV EDX, dword ptr[ESP+4]');
              wASM('  ADD ESP,8');
            End;
        End;
      End;
    End;

    // das Zwischenergebnis edx zurück nach eax
    Case Result_Type Of
      tBoolean: wASM('  mov al,dl');
      tByte: wASM('  mov al,dl');
      tchar: wASM('  mov al,dl');

      tInt16: wASM('  mov ax,dx');
      tInt32: wASM('  mov eax,edx');
      tsingle: wASM('  mov eax,edx');
      tClass: wASM('  mov eax,edx');
      tString: wASM('  mov eax,edx');

    End;
  End;

  Result := Result_Type;
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Condition;
Var
  loc_iLabelCounter : word;
  Loc_Typ, Express_Typ: TType;
Begin
  Loc_iLabelCounter := iLabelCounter;

  If tkToken = ttNot Then
    NextToken;

  Loc_Typ := Call_Expression;
  Case Loc_Typ Of
    tInt16: wASM('PUSH ax');
    tInt32, tClass, tSingle: wASM('PUSH eax');
  End;

  Case tkToken Of
    ttEqu:                              // "="
      Begin
        NextToken;
        Express_Typ := Call_Expression;

        Loc_Typ := CastType(Loc_Typ, Express_Typ);

        If Loc_Typ = TNone Then
          Error('Typen Fehler');

        Case Loc_Typ Of
          tInt16:
            Begin
              wASM('pop dx');
              wASM('cmp dx,ax');
              wASM('mov ax,1');
              wASM('je CM' + InttoStr(iLabelCounter + 1));
            End;
          tInt32, tClass:
            Begin
              wASM('pop edx');
              wASM('cmp edx,eax');
              wASM('mov ax,1');
              wASM('je CM' + InttoStr(iLabelCounter + 1));
            End;
          tString:
            Begin
              wASM('pop dx');
              wASM('Push esi');         // Objektpointer speichern
              wASM('mov si,dx');
              wASM('mov di,ax');
              wASM('call posstr');
              wASM('Pop esi');          // Objektpointer wiederherstellen
              wASM('cmp al,0');
              wASM('mov ax,1');
              wASM('jne CM' + InttoStr(iLabelCounter + 1));
            End;
        End;

      End;
    ttLess:                             // "<"
      Begin
        NextToken;
        Express_Typ := Call_Expression;

        Loc_Typ := CastType(Loc_Typ, Express_Typ);

        If Loc_Typ = TNone Then
          Error('Typen Fehler');

        wASM('pop dx');
        wASM('cmp dx,ax');
        wASM('mov ax,1');
        wASM('jl CM' + InttoStr(iLabelCounter + 1));
      End;
    ttEquLess:
      Begin
        NextToken;
        Express_Typ := Call_Expression;

        Loc_Typ := CastType(Loc_Typ, Express_Typ);

        If Loc_Typ = TNone Then
          Error('Typen Fehler');
        wASM('pop dx');
        wASM('cmp dx,ax');
        wASM('mov ax,1');
        wASM('jle CM' + InttoStr(iLabelCounter + 1));
      End;
    ttNotEqu:
      Begin
        NextToken;
        Express_Typ := Call_Expression;

        Loc_Typ := CastType(Loc_Typ, Express_Typ);

        If Loc_Typ = TNone Then
          Error('Typen Fehler');

        Case Loc_Typ Of
          tInt16:
            Begin
              wASM('pop dx');
              wASM('cmp dx,ax');
              wASM('mov ax,1');
              wASM('jne CM' + InttoStr(iLabelCounter + 1));
            End;
          tInt32, tClass:
            Begin
              wASM('pop Edx');
              wASM('cmp Edx,Eax');
              wASM('mov ax,1');
              wASM('jne CM' + InttoStr(iLabelCounter + 1));
            End;
          tString:
            Begin
              wASM('pop dx');
              wASM('Push esi');         // Objektpointer speichern
              wASM('mov si,dx');
              wASM('mov di,ax');
              wASM('call posstr');
              wASM('cmp al,0');
              wASM('mov ax,1');
              wASM('Pop esi');          // Objektpointer wiederherstellen
              wASM('je CM' + InttoStr(iLabelCounter + 1));
            End;
        End;
      End;
    ttEquGreater:                       // ">="
      Begin
        NextToken;
        Express_Typ := Call_Expression;

        Loc_Typ := CastType(Loc_Typ, Express_Typ);

        If Loc_Typ = TNone Then
          Error('Typen Fehler');
        wASM('pop dx');
        wASM('cmp dx,ax');
        wASM('mov ax,1');
        wASM('jge CM' + InttoStr(iLabelCounter + 1));
      End;
    ttGreater:                          // ">"
      Begin
        NextToken;

        Express_Typ := Call_Expression;
        Loc_Typ := CastType(Loc_Typ, Express_Typ);
        If Loc_Typ = TNone Then
          Error('Typen Fehler');

        wASM('pop dx');
        wASM('cmp dx,ax');
        wASM('mov ax,1');
        wASM('jg CM' + InttoStr(iLabelCounter + 1));
      End;
  Else
    Begin

      wASM('pop ax');
      wASM('jmp CM' + InttoStr(iLabelCounter + 1));
    End
  End;

  // Label CMxxx, wenn Bedingung erfüllt
  wASM('xor ax,ax');
  wASM('CM' + InttoStr(iLabelCounter + 1) + ':');

  // Label-Zähler erhöhen
  Inc(iLabelCounter);

  // AND | OR
  If tkToken = ttAnd Then
  Begin
    NextToken;
    wASM('push ax');
    Call_Condition;

    wASM('pop dx');
    wASM('and ax,dx');
    wASM('mov ax,1');
    wASM('jnz CM' + InttoStr(iLabelCounter + 1));
    wASM('xor ax,ax');
    wASM('CM' + InttoStr(iLabelCounter + 1) + ':');
    Inc(iLabelCounter);
  End
  Else If tkToken = ttOr Then
  Begin
    NextToken;
    wASM('push ax');
    Call_Condition;
    wASM('pop dx');
    wASM('or ax,dx');
    wASM('mov ax,1');
    wASM('jnz CM' + InttoStr(iLabelCounter + 1));
    wASM('xor ax,ax');
    wASM('CM' + InttoStr(iLabelCounter + 1) + ':');
    Inc(iLabelCounter);
  End;
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:  Zuweisung
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Assignment;
Var
  sAdress           : String;
  Loc_Typ1, Loc_Typ2, Result_Type: TType;
  MyVar             : TcVarEnter;
Begin

  // Der Basepointer (SELF) sicher verwahren...
  // leider doppelt, (LS) und (RS) haben beide das Recht nach Self auflösen
  // Zu können
  wASM('  Push ESI ; Entry Call_Assignment 1');
  wASM('  Push ESI ; Entry Call_Assignment 2');

  Loc_Typ1 := Call_Ident(sAdress, sClassName); // LS
  Result_Type := Loc_Typ1;

  // evtl.Zuweisung mit LS := RS;
  If tkToken = ttColon Then             // ":"
  Begin
    If NextToken = ttEqu Then           // "="
    Begin
      NextToken;
      // über den Stack den ESI Basepointer zu laden ist etwas
      // umständlich, es geht aber...
      wASM('  POP EBX ; LOAD Basepointer ESI'); // den Class - Basepointer von SELF laden und zwischenspeichern
      wASM('  Push ESI ; Store LS');    // den Class - Basepointer der linken Seite speichern
      wASM('  mov ESI ,EBX; Store LS'); // Self Basisadresse nach ESI für RS

      wASM('  XOR EAX,EAX');            // benötigte Register leeren
      wASM('  XOR EDX,EDX');

      Loc_Typ2 := Call_Expression;      // RS

      Result_Type := CastType(Loc_Typ1, Loc_Typ2);

      wASM('  POP ESI ; Restore LS');   // den Class - Basepointer der linken Seite laden

      If Result_Type = TNone Then
        Error('Typenfehler')
      Else
        Case Result_Type Of
          tInt16: wASM('  mov word ptr' + sAdress + ', ax');
          tInt32: wASM('  mov dword ptr ' + sAdress + ', eax');

          tBoolean: wASM('  mov byte ptr ' + sAdress + ', al'); //??
          tByte: wASM('  mov byte ptr ' + sAdress + ' , al'); //??
          tChar: wASM('  mov byte ptr ' + sAdress + '  , al'); //??
          tU_Int16: wASM('  mov ' + sAdress + ', ax'); //??
          tU_Int32: wASM('  mov ' + sAdress + ', eax'); //??
          tPointer: wASM('  mov ' + sAdress + ', eax'); //??
          tSingle: wASM('  mov dword ptr ' + sAdress + ', eax'); //??
          tString: wASM('  mov dword ptr ' + sAdress + ', eax');
          tArray: wASM('  mov  dword ptr ' + sAdress + ', eax'); //??
          tClass: wASM('  mov dword ptr ' + sAdress + ', eax'); //??
        End;
    End;
  End
  Else
    // wenn der Basepointer nicht gebraucht wird, dann muss er dennoch vom Stack
    // genommen werden
    wASM('  POP ESI ; RS not used');

  wASM('  Pop ESI ; Exit Call_Assignment');
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Statement;
Begin
  Case tkToken Of
    ttSemicolon: ;
    ttEmpty: ;
    ttMain: Call_Main;
    ttClass: Call_Class;
    ttConst: Call_Const;
    ttVar: Call_Var;
    ttBegin: Call_Begin;
    ttIdentifier: Call_Assignment;
    ttIf: Call_If;
    ttWhile: Call_While;
    ttRepeat: Call_Repeat;
    ttFor: Call_For;
    ttWriteln, ttWrite: Call_Write;
    ttRead, ttReadln: Call_Read;
    ttEnd: ;
  Else
    Error('unknown Token: ' + Self.sToken);
  End;

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Const;
Begin

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Var;
Begin

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.Call_Ident(Var sAdress: String; sClass: String): tType;
Var
  sIdent            : String;
  cVarEnter         : TcVarEnter;
  iOffset           : Integer;
Begin

  Result := tNone;
  sIdent := sToken;

  If sClass <> '' Then
    cVarEnter := oVarLIst.FindClassIdent(sIdent, sClass)
  Else
    cVarEnter := oVarLIst.Find(sIdent);

  If cVarEnter <> Nil Then
  Begin

    LoadVarOffset(cVarEnter, sAdress, iOffset); // lade den Offset der Variablen
    NextToken;

    // Array? [...]
    If (TKToken = ttLS) Then
    Begin
      NextToken;
      Result := Call_Expression;

      // wenn das Feldes nicht bei Null beginnt....
      // dann Offset berechnen
      If cVarEnter.iArrayDims[0].iArrayFrom <> 0 Then
      Begin
        wASM(' SUB EAX, ' + InttoStr(cVarEnter.iArrayDims[0].iArrayFrom));
      End;

      // wenn die Anzahl des Ergebnistyps größer als 1 Byte, dann multip.
      If cByteLen[cVarEnter.ArrayTyp] > 1 Then
      Begin
        wASM(' MOV EDX, ' + InttoStr(cByteLen[cVarEnter.ArrayTyp]));
        wASM(' MUL EDX');
      End;
      wASM(' mov ECX,EAX');
      sAdress := StringReplace(sAdress, '[', '', [rfReplaceAll]);
      sAdress := StringReplace(sAdress, ']', '', [rfReplaceAll]);

      //cVarEnter.iArrayDims[0].iArrayFrom

      sAdress := '[' + sAdress + ' + ECX]';

      If (TKToken = ttRS) Then
        NextToken;

    End;

    // [esi + Offset], wenn Member
    // [ebp - Offset], wenn Parameter
    // [ebp + Offset], wenn lok Variable

    If (TKToken = ttPoint) And          // Class.Member
    (cVarEnter.tpType = tClass) Then    // Auflösung
    Begin
      wASM(' mov eax,' + sAdress);      // lade Klassen - Instanz - Zeiger nach ESI // Änderung 17.06.2009
      wASM(' mov esi,eax');

      NextToken;

      Result := Call_Ident(sAdress, cVarEnter.sClassType); // Member oder Subklasse auflösen
    End
    Else
    Begin
      If lowercase(cVarEnter.sName) = 'result' Then
      Begin
        Result := StringToType(cVarEnter.sClassType) // Rückgabe des Datentyps
      End
      Else
        Result := cVarEnter.tpType;     // Rückgabe des Datentyps

      Case cVarEnter.tpType Of          // Member gefunden
        tBoolean: ;
        tByte: ;
        tChar: ;
        tU_Int16: ;
        tU_Int32: ;
        tInt16: ;
        tInt32: ;
        tPointer: ;
        tSingle: ;
        tString: ;
        tArray: Result := cVarEnter.ArrayTyp;
        tClass:
          Begin
          End;
        tFunction:
          Begin
            Call_ProcedureParameters(cVarEnter);
            wAsm('Call ' + cVarEnter.sClass + '_' + cVarEnter.sProcedure);
            sAdress := '';
            Result := cVarEnter.Resulttype;
          End;
        tProcedure:
          Begin
            Call_ProcedureParameters(cVarEnter);
            wAsm('Call ' + cVarEnter.sClass + '_' + cVarEnter.sProcedure);
          End;
      End;

    End;

  End;

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Begin;
Begin
  While Not bError And (tkToken <> ttEnd) Do
  Begin
    NextToken;
    Call_Statement;
  End;

  NextToken;
End;
{*******************************************************************************
Procedure:  TCompiler.Call_Main
Description: Hauptprogramm
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Main;
Var
  Descriptor        : TDescriptor;
Begin
  wAsm('include .\include\win32.inc');

  wAsm('_Main:');
  wAsm('$_Main proc');

  wAsm('push    offset lppaint');
  wAsm('push    [hwnd]');
  wAsm('call    BeginPaint');
  wAsm('mov     [theDC], eax');

  sClassName := '';

  If NextToken <> ttSemicolon Then
    ParseError(ttSemicolon)
  Else
    NextToken;

  // lokale Variablen
  If tkToken = ttVar Then
  Begin
    Repeat
      Self.EmptyDescriptor(Descriptor);
      Descriptor.tpVisibility := tmGlobal;
      NextToken;
      Call_Deklaration(Descriptor);

    Until bError Or (tkToken In [ttBegin]);
  End;

  If tkToken In [ttSemicolon, ttBegin] Then
  Begin
    Call_Statement;
  End
  Else
    ParseError(ttSemicolon);
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_For;
Var
  iLoc_LabelCounter : Integer;
  sIdent            : String;
  cVarEnter         : TcVarEnter;
  iOffset           : Integer;
  sAdress           : String;
  L_Type            : TType;
Begin
  NextToken;
  sIdent := sToken;
  inc(iLabelCounter, 2);                // es werden für eine While-Schleife 4 Labels benötigt

  If tkToken = ttIdentifier Then        (*Identifier!!!*)
  Begin
    cVarEnter := oVarLIst.FindClassIdent(sIdent, self.sClassName);

    If cVarEnter <> Nil Then
    Begin
      If (cVarEnter.tpType = tInt16) Or (cVarEnter.tpType = tInt32) Then
      Begin

        LoadVarOffset(cVarEnter, sAdress, iOffset); // lade den Offset der Variablen
        L_Type := cVarEnter.tpType;     // den DatenType merken

        NextToken;
        If tkToken = ttColon Then       // ":"
        Begin
          If NextToken = ttEqu Then     // "="
          Begin
            NextToken;
            wASM('XOR EAX,EAX');        // Assign Value
            Call_Expression;            // From Expression

            Case L_Type Of
              tInt16: wASM('Mov ' + sAdress + ',AX'); // Assign Value
              tInt32: wASM('Mov ' + sAdress + ',EAX'); // Assign Value
            End;

            If tkToken = ttTo Then
            Begin
              NextToken;
              Call_Expression;          // To Expression

              Case L_Type Of
                tInt16: wASM('Mov DX,AX'); // Store Value in EDX
                tInt32: wASM('Mov EDX,EAX'); // Store Value in EDX
              End;

              If tkToken = ttdo Then    // do
              Begin
                NextToken;

                iLabelCounter := iLabelCounter + 1;
                iLoc_LabelCounter := iLabelCounter;
                // Label für Rücksprung and den Anfang
                wASM('W' + InttoStr(iLabelCounter) + ' label near');

                wASM('Push EDX');
                Call_Statement;
                wASM('Pop EDX');

                Case L_Type Of
                  tInt16:
                    Begin
                      wASM('Mov AX, ' + sAdress);
                      wASM('INC AX');
                      wASM('Mov ' + sAdress + ',AX');
                      // Wenn Bedingung WAHR, dann
                      wASM('cmp DX,AX');
                      wASM('jge W' + InttoStr(iLoc_LabelCounter));
                    End;
                  tInt32:
                    Begin
                      wASM('Mov EAX, ' + sAdress);
                      wASM('INC EAX');
                      wASM('Mov ' + sAdress + ',EAX');
                      // Wenn Bedingung WAHR, dann
                      wASM('cmp EDX,EAX');
                      wASM('jge W' + InttoStr(iLoc_LabelCounter));
                    End;
                End;

              End;
            End
          End
          Else
            self.Error('Laufvariable muss vom Typ Integer sein!');
        End;
      End;
    End;
  End;
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Function(Descriptor: TDescriptor);
Begin

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Procedure(Descriptor: TDescriptor);
Var
  L_Descriptor      : TDescriptor;
  L_ResultDescriptor: TDescriptor;
  iByteLen          : Integer;
  bIsFunction       : Boolean;
  iOffset           : Integer;
  bHasLocValues     : Boolean;
Begin
  bIsFunction := tkToken = ttFunction;
  bHasLocValues := False;

  If NextToken = ttIdentifier Then
  Begin

    Descriptor.sName := sToken;
    Descriptor.sProcedure := sToken;

    // schreibe Procedure Label: KLASSE_PROZEDUR
    wASM(Descriptor.sClass + '_' + Descriptor.sName + ' proc near');

    wASM('push ebp');
    wASM('mov ebp,esp');
    wASM('push ebp');

    oVarList.Add(Descriptor);           // Prozedur als solche merken

    NextToken;

    If tkToken = ttLB Then              // '('
    Begin
      Repeat
        L_Descriptor := Descriptor;

        NextToken;

        If tkToken = ttVar Then         // variablenparameter
        Begin
          NextToken;
          L_Descriptor.tpVisibility := tmVarParameter;
        End
        Else
          L_Descriptor.tpVisibility := tmParameter; // Parameter

        Call_Deklaration(L_Descriptor); // Deklarationteil abarbeiten

        If (tkToken <> ttRB) And (tkToken <> ttSemicolon) Then // ;
          ParseError(ttSemicolon);

      Until bError Or (tkToken = ttRB); //')'

      NextToken;
    End;

    // wenn Funktion
    If bIsFunction Then
    Begin
      If tkToken = ttColon Then
      Begin
        L_ResultDescriptor := Descriptor;
        L_ResultDescriptor.tpVisibility := tmLocal;
        L_ResultDescriptor.sName := 'result';
        Call_DataType(L_ResultDescriptor); // Datentyp laden und ab in den Vartable
        NextToken;
        bHasLocValues := True;
      End
      Else
        Error('":" missing');
    End;

    If tkToken = ttSemicolon Then
    Begin
      NextToken;
    End;

    // lokale Variablen
    If tkToken = ttVar Then
    Begin
      Repeat
        L_Descriptor := Descriptor;
        L_Descriptor.tpVisibility := tmLocal;
        NextToken;
        Call_Deklaration(L_Descriptor);

      Until bError Or (tkToken In [ttBegin]);

      bHasLocValues := True;

    End;

    // Wenn es lokale Variablen gibt oder Funktion....
    If bHasLocValues Then
    Begin
      // Offsets berechnen
      oVarList.CalcClassOffsets(Descriptor.sClass);

      iByteLen :=
        oVarList.FindClassIdent(Descriptor.sName,
        Descriptor.sClass).iLocalStack;

      wASM('SUB ESP,' + inttoStr(iByteLen));
    End;

    // Statement
    If tkToken In [ttSemicolon, ttBegin] Then
    Begin
      Call_Statement;
    End
    Else
      ParseError(ttSemicolon);

    // Rückgabewert nach EAX
    If bIsFunction Then
    Begin
      // Result nach EAX laden
      iOffset := oVarList.FindClassIdent(L_ResultDescriptor.sName,
        L_ResultDescriptor.sClass).iOffset;

      wASM('mov EAX, DWord ptr [ebp - ' + InttoStr(iOffset) + ']');
    End;

    wASM('mov esp,ebp');
    wASM('pop ebp');

    // Stackgröße
    iByteLen :=
      oVarList.FindClassIdent(Descriptor.sName, Descriptor.sClass).iStackSize;
    // Return und Stackkorrektur:
    // die Übergabeparameter, die beim Aufruf auf den Stack gelegt wurden,
    // werden mit der iStackSize korrigiert
    wASM('RET ' + inttoStr(iByteLen));  // Korrektur des Parameterstacks
    wASM(Descriptor.sClass + '_' + Descriptor.sName + ' ENDP');

  End
  Else
    ParseError(ttIdentifier);
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_If;
Var
  iLoc_LabelCounter : Integer;
Begin
  NextToken;

  inc(iLabelCounter);                   // es wird für eine If-Bed 1 Label benötigt

  iLoc_LabelCounter := iLabelCounter;

  Call_Condition;
  Inc(iLabelCounter);

  // Wenn Bedingung erfüllt, dann...
  wASM('cmp ax,1');
  wASM('je I' + InttoStr(iLabelCounter));
  wASM('jmp I' + InttoStr(iLoc_LabelCounter));
  wASM('I' + InttoStr(iLabelCounter) + ':');

  If tkToken = ttThen Then
    NextToken
  Else
    ParseError(ttThen);

  iLabelCounter := iLabelCounter + 6;   // 6 Labels reservieren

  Call_Statement;
  // Am Ende des Blocks, an das Ende der If-Anweisung springen
  wASM('jmp I' + InttoStr(iLoc_LabelCounter + 6));

  // Bedingung Nicht erfüllt... Anfang des Else-Zweiges
  wASM('I' + InttoStr(iLoc_LabelCounter) + ' label near');

  If Not bError And (tkToken = ttElse) Then
  Begin
    NextToken;
    Call_Statement;
  End;

  // Label Ende der IF/ELSE-Anweisung
  wASM('I' + InttoStr(iLoc_LabelCounter + 6) + ' label near');
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Repeat;
Begin

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_While;
Var
  iLoc_LabelCounter : Integer;
Begin
  NextToken;

  inc(iLabelCounter, 4);                // es werden für eine While-Schleife 4 Labels benötigt

  iLoc_LabelCounter := iLabelCounter;

  // Label für Rücksprung and den Anfang
  wASM('W' + InttoStr(iLabelCounter - 1) + ' label near');

  // Bedingung
  Call_Condition;

  iLabelCounter := iLabelCounter + 1;
  // Wenn Bedingung WAHR, dann
  wASM('cmp ax,1');
  wASM('je W' + InttoStr(iLabelCounter));
  // Sonst an das Ende Springen
  wASM('jmp W' + InttoStr(iLoc_LabelCounter));
  wASM('W' + InttoStr(iLabelCounter) + ':');

  If tkToken = ttdo Then                (*ohne DO kein Statement!!!*)
  Begin
    NextToken;
    Call_Statement;
  End;

  wASM('jmp W' + InttoStr(iLoc_LabelCounter - 1));
  wASM('W' + InttoStr(iLoc_LabelCounter) + ' label near');
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Read;
Begin

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Write;
Var
  tkStoreToken      : Integer;
  L_Type            : TType;
Begin
  tkStoreToken := tkToken;
  NextToken;

  wASM('Push esi');                     // Objektpointer speichern

  If tkToken = ttLB Then
    Repeat
      NextToken;

      L_Type := Call_Expression;

      Case L_Type Of
        tChar:
          Begin
            wASM('Call $_CharWrite');
          End;
        tString:
          Begin
            wASM('mov esi,eax');
            //wASM('call print');
            wASM('Call $_StringWrite');
          End;
        tInt16, tInt32, TPointer, TClass:
          Begin
            inc(iLabelCounter);

            wASM('SUB ESP,20');         // Speicher definieren
            wASM('mov ESI,ESP');

            wASM('push EAX               ; Store Result');
            wASM('CLD                    ; clear direction flag => increment EDI');
            wASM('MOV ECX, 20            ; Count init ECX for REP');
            wASM('MOV EAX, 32            ; Char init AL for STOSB');
            wASM('MOV EDI, ESI           ; dest init EDI for STOSB');
            wASM('REP STOSB              ; Repeat: Leerzeichen (#32) in den Speicher laden');
            wASM('pop EAX                ; Load Result');
            wASM('mov Byte ptr[ESI],1    ; String Byte 0');
            wASM('mov Byte ptr[ESI+1],16 ; String Anzahl');

            wASM('ADD ESI,18			    ; Zeiger ans Ende (Zeichen rechts ausrichten)');
            wASM('XOR ECX, ECX		    ; Basis 10');
            wASM('XOR EDX, EDX		    ; 0 ');
            wASM('Call $_Int32ToStr	  ; Konvertierung');

            wASM('mov ESI, ESP');
            wASM('Call $_StringWrite');
            wASM('ADD ESP,20');         // Speicher freigeben
          End;
        tSingle:
          Begin
            inc(iLabelCounter);

            wASM('SUB ESP,20');         // Speicher definieren
            wASM('mov ESI,ESP');

            // die 20 Byte mit Leerzeichen (#32) füllen...
            wASM('push EAX               ; Store Result');
            wASM('CLD                    ; clear direction flag => increment EDI');
            wASM('MOV ECX, 20            ; Count init ECX for REP');
            wASM('MOV EAX, 32            ; Char init AL for STOSB');
            wASM('MOV EDI, ESI           ; dest init EDI for STOSB');
            wASM('REP STOSB              ; Repeat ');
            wASM('pop EAX                ; Load Result');

            wASM('mov Byte ptr[ESI],1    ; Byte 0');
            wASM('mov Byte ptr[ESI+1],16 ; Byte 1: String-Länge');

            wASM('ADD ESI,18			    ; Byte 2..20: Zeiger ans Ende (Zeichen rechts ausrichten)');
            wASM('XOR ECX, ECX		    ; Basis 10');
            wASM('XOR EDX, EDX		    ; EDX=0 ');
            wASM('Call $_SingleToStr	  ; Konvertierung');

            wASM('mov ESI, ESP        ; Zeiger kopieren...');
            wASM('Call $_StringWrite');
            wASM('ADD ESP,20');         // Speicher freigeben
          End;
      Else
        Error('Datentyp kann nicht ausgedruckt werden');
      End;
    Until tkToken <> ttCol;             // ","

  If tkStoreToken = ttWriteln Then
  Begin
    wASM('Call $_NLN');
  End;

  wASM('Pop esi');                      // Objekt Pointer zurückspeichern

  If tkToken = ttRB Then
    NextToken;
End;

{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Class;
Var
  Descriptor        : TDescriptor;
Begin
  If NextToken = ttIdentifier Then
  Begin
    sClassName := sToken;

    If NextToken = ttOf Then
    Begin
      // Beschreibung der Klasse
      Descriptor.sClass := sClassName;
      Descriptor.tpType := tClass;
      Descriptor.tpVisibility := tmType;
      Descriptor.tkKind := tkNone;
      Descriptor.ResultType := tNone;

      // für jede Ableitung der Klasse, einen Eintrag in der VarList
      While Not bError And (NextToken <> ttImplements) Do
      Begin
        If tkToken = ttBase Then
          Descriptor.sName := ''
        Else
          Descriptor.sName := sToken;
        oVarList.Add(Descriptor);
      End;

      // Implements
      If tkToken = ttImplements Then
      Begin
        EmptyDescriptor(Descriptor);
        Descriptor.sClass := sClassName;
        NextToken;
        // die Implementation der Klasse....
        Call_Implementation(Descriptor);
      End
      Else
        ParseError(ttImplements);
    End
    Else
      ParseError(ttOf);
  End
  Else
    ParseError(ttIdentifier)
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Implementation(Descriptor: TDescriptor);
Begin
  Repeat
    Case tkToken Of
      ttPrivate:                        // private Deklaration
        Begin
          Repeat
            If Not (tkToken In [ttProcedure, ttFunction]) Then // Ausnahme
              NextToken;

            Descriptor.tkKind := tkPrivate; // PRIVATE
            Descriptor.tpVisibility := tmMember; // MEMBER-Variable

            // durchlaufe den Deklarationsteil der Klasse bis Ende
            If Not (tkToken In [ttEnd, ttPublic]) Then
            Begin
              Call_Deklaration(Descriptor);

              If Not tkToken In [ttSemicolon, ttEnd, ttPublic] Then // ;
                ParseError(ttSemicolon);
            End;

          Until bError Or (tkToken In [ttEnd, ttPublic]);
        End;
      ttPublic:
        Begin

          Repeat
            NextToken;

            Descriptor.tkKind := tkPublic; // öffentliche VAriablen
            Descriptor.tpVisibility := tmMember; // Member- VAriable

            // durchlaufe den Deklarationsteil der Klasse bis Ende
            If Not (tkToken In [ttEnd, ttPrivate]) Then
            Begin
              Call_Deklaration(Descriptor);

              If Not tkToken In [ttSemicolon, ttEnd, ttPrivate] Then // ;
                ParseError(ttSemicolon);
            End;

          Until bError Or (tkToken In [ttEnd, ttPrivate]);
        End;
      ttEnd:
        Begin
        End;
    Else
      ParseError(ttPublic);
    End;
  Until bError Or (tkToken = ttEnd);
End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Deklaration(Descriptor: TDescriptor);
Begin
  Repeat
    Case tkToken Of
      ttIdentifier:                     // Ident : DataType
        Begin
          Descriptor.sName := sToken;

          If NextToken = ttColon Then   // :
          Begin
            Call_DataType(Descriptor);  // Datentyp laden und ab in den Vartable
          End
          Else
            ParseError(ttColon);

          NextToken;
        End;
      ttProcedure:                      // Procedure
        Begin
          Descriptor.tpType := tProcedure;
          Call_Procedure(Descriptor);   // Parameter auflösen
        End;
      ttFunction:                       // Procedure
        Begin
          Descriptor.tpType := tFunction;
          Call_Procedure(Descriptor);   // Parameter auflösen
        End;
    End;
  Until Not (tkToken In [ttProcedure, ttFunction]);

End;
{*******************************************************************************
Procedure:  TCompiler.
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.Call_DataType(Descriptor: TDescriptor): TType;
Var
  ExpressionType    : TType;
  iArrayFrom        : Integer;
  iArrayTo          : Integer;
  SubDescriptor     : TDescriptor;
  iNeg              : Integer;
Begin
  Case NextToken Of
    ttIdentifier:                       // Datentyp ist eine ander Klasse
      Begin
        Descriptor.tpType := tClass;
        Descriptor.sClassType := sToken;
      End;
    ttBoolean:
      Begin
        Descriptor.tpType := tBoolean;
        Descriptor.sClassType := sToken;
      End;
    ttByte:
      Begin
        Descriptor.tpType := tByte;
        Descriptor.sClassType := sToken;
      End;
    ttChar:
      Begin
        Descriptor.tpType := tChar;
        Descriptor.sClassType := sToken;
      End;
    ttU_Int16:
      Begin
        Descriptor.tpType := tU_Int16;
        Descriptor.sClassType := sToken;
      End;

    ttU_Int32:
      Begin
        Descriptor.tpType := tU_Int32;
        Descriptor.sClassType := sToken;
      End;
    ttInt16:
      Begin
        Descriptor.tpType := tInt16;
        Descriptor.sClassType := sToken;
      End;
    ttInt32:
      Begin
        Descriptor.tpType := tInt32;
        Descriptor.sClassType := sToken;
      End;
    ttPointer:
      Begin
        Descriptor.tpType := tPointer;
        Descriptor.sClassType := sToken;
      End;
    ttSingle:
      Begin
        Descriptor.tpType := tSingle;
        Descriptor.sClassType := sToken;
      End;
    ttString:
      Begin
        Descriptor.tpType := tString;
        Descriptor.ArrayTyp := tChar;
        Descriptor.iArrayDim := 1;
        Descriptor.iArrayElements := 81; // [0..80]
        Setlength(Descriptor.iArrayDims, 80);
        Descriptor.iArrayDims[0].iArrayFrom := 0;
        Descriptor.iArrayDims[0].iArrayTo := 80;
        Descriptor.sClassType := sToken;
      End;
    ttArray:
      Begin
        Descriptor.tpType := tArray;
        NextToken;
        // [..] of ...
        If tkToken = ttLS Then
        Begin                           // [
          NextToken;
          Descriptor.iArrayDim := 0;
          Descriptor.iArrayElements := 1;

          Repeat
            If tkToken In [ttMinus, ttPlus] Then
            Begin
              iNeg := -1;
              NextToken;
            End
            Else
              iNeg := +1;

            If tkToken In [ttInt16, ttint32] Then
            Begin

              iArrayFrom := StrToInt(sToken) * iNeg;

              If NextToken = ttDoublePoint Then {..}
              Begin
                If NextToken In [ttInt16, ttint32] Then
                Begin

                  If tkToken In [ttMinus, ttPlus] Then
                  Begin
                    iNeg := -1;
                    NextToken;
                  End
                  Else
                    iNeg := +1;
                  iArrayTo := StrToInt(sToken) * iNeg;

                  If iArrayTo < iArrayFrom Then
                    Error('Array: Feldgrenzen nicht plausibel');

                  // speichere die Arraygrenzen ab....
                  inc(Descriptor.iArrayDim);
                  Setlength(Descriptor.iArrayDims, Descriptor.iArrayDim);
                  Descriptor.iArrayElements := Descriptor.iArrayElements * (iArrayTo - iArrayFrom + 1);
                  Descriptor.iArrayDims[Descriptor.iArrayDim - 1].iArrayFrom := iArrayFrom;
                  Descriptor.iArrayDims[Descriptor.iArrayDim - 1].iArrayTo := iArrayTo;

                  NextToken;
                End;
              End;
            End;

            If tkToken = ttCol Then
              NextToken;

          Until (tkToken = ttRS) Or (tkToken = ttEmpty);

          If tkToken = ttRS Then
          Begin
            If NextToken = ttOf Then
            Begin
              SubDescriptor.sName := '';

              Descriptor.ArrayTyp := Call_DataType(SubDescriptor); //TokenToType(TKToken);
            End;
          End;

        End;

      End;
  Else
    Error('Datentyp erwartet');
  End;

  Descriptor.Resulttype := Descriptor.tpType;

  // Beschreibung der Variablen in die Liste
  If Not bError Then
    If Descriptor.sName <> '' Then
      oVarList.Add(Descriptor);

  Result := Descriptor.Resulttype;
End;

{*******************************************************************************
Procedure:  TCompiler.
Description: Leere den Descriptor
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.EmptyDescriptor(Var Descriptor: TDescriptor);
Begin
  Descriptor.sClass := '';
  Descriptor.sName := '';
  Descriptor.sProcedure := '';
  Descriptor.sClassType := '';
  Descriptor.tpType := tNone;
  Descriptor.tpVisibility := tmNone;
  Descriptor.tkKind := tkNone;
End;

{*******************************************************************************
Procedure:  TCompiler.LoadVarOffset
Description:  Lade den Variablen Offset in Abhängigkeit von der Sichtbarkeit
              der Variablen
Author: MHS
Date:   29.07.2007
History: V1.0

// lade den Offset der Variablen
// mov ax,[Var], wenn globale Variable
// mov ax,[si + Offset], wenn Member
// mov ax,[bp - Offset], wenn Parameter
// mov ax,[bp + Offset], wenn lok Variable
*******************************************************************************}

Function TCompiler.LoadVarOffset(cVarEnter: TcVarEnter; Var sAdress: String; Var
  iOffset: Integer):
  Boolean;
Begin
  Case cVarEnter.tpVisibility Of
    tmNone: ;
    tmConstant: ;
    tmType: ;
    tmGlobal:
      Begin
        sAdress := '[' + cVarEnter.sName + ']';
        iOffset := 0;
      End;
    tmLocal:
      Begin
        sAdress := '[ebp - ' + InttoStr(cVarEnter.iOffset) + ']';
        iOffset := -cVarEnter.iOffset;
      End;
    tmMember:
      Begin
        sAdress := '[esi + ' + InttoStr(cVarEnter.iOffset) + ']'; // Beachte, dass der Offset  // Änderung 17.06.2009
        iOffset := cVarEnter.iOffset;
      End;
    tmVarParameter:
      Begin
        sAdress := '[ebp + ' + InttoStr(cVarEnter.iOffset) + ']';
        iOffset := cVarEnter.iOffset;
      End;
    tmParameter:
      Begin
        sAdress := '[ebp + ' + InttoStr(cVarEnter.iOffset) + ']';
        iOffset := cVarEnter.iOffset;
      End;
  End;
End;
{*******************************************************************************
Procedure:  TCompiler.
Description: lade den Wert einer Variablen
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.LoadVarValue(cVarEnter: TcVarEnter): Boolean;
Begin
  Case cVarEnter.tpType Of
    tNone: ;
    tBoolean: wASM('  mov al,byte ptr [esi]');
    tByte: wASM('  mov al,byte ptr [esi]');
    tChar: wASM('  mov al,byte ptr [esi]');
    tU_Int16: wASM('  mov ax, word ptr [esi]');
    tU_Int32: wASM('  mov eax, dword ptr[esi]');
    tInt16: wASM('  mov ax, word ptr [esi]');
    tInt32: wASM('  mov eax, dword ptr [esi]');
    tPointer: wASM('  mov eax, dword ptr [esi]');
    tSingle: wASM('  mov eax, dword ptr [esi]');
    tString: wASM('  mov eax, dword ptr [esi]');
    tArray: wASM('  mov eax, dword ptr [esi]');
    tClass: wASM('  mov eax, dword ptr  [esi]');
    tFunction: wASM('  mov eax, dword ptr[esi]');
    tProcedure: wASM('  mov eax,dword ptr [esi]');
  End
End;
{*******************************************************************************
Procedure:  OnNewLine.OnNewLine
Description: Zeile wird übersetzt
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.OnNewLine(sActualLine: String);
Begin
  wAsm(';' + Trim(sActualLine));
End;
{*******************************************************************************
Procedure:  OnNewLine.OnNewLine
Description: Zeile wird übersetzt
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Prolog;
Begin
  wAsm(';' + sFilename + ' AT ' + DateTimeToStr(NOW));
  wAsm('.486');
  wAsm('locals');
  wAsm('jumps');
  wAsm('.model flat,STDCALL');
  wAsm(';============== Prolog Ended');
  wAsm('Include .\include\Prolog.asm');
End;
{*******************************************************************************
Procedure:  OnNewLine.OnNewLine
Description: Zeile wird übersetzt
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_Epilog;
Var
  iVarIDX           : Integer;
  L_VarEnter        : TcVarEnter;
Begin

  wAsm('push    offset lppaint');
  wAsm('push    [hwnd]');
  wAsm('call    EndPaint');
  wASM('RET');
  wASM('$_Main endp');

  wAsm('Include .\include\Lib.asm');
  wASM('.data');
  wASM('copyright        db ''(c)M.Hoeller-Schlieper CubeOS 2007'',0');

  wASM('newhwnd          dd 0');
  wASM('lppaint          PAINTSTRUCT <?>');
  wASM('msg              MSGSTRUCT   <?>');
  wASM('wc               WNDCLASS    <?>');
  wASM('hInst            dd 0');
  wASM('szTitleName      db ''CubeOS - Test Environment''');
  wASM('szClassName      db ''CubeOS'',0');
  wASM('_Y				       dd 5');
  wASM('_X				       dd 5');
  wASM('_TextHeight		   dd 15');

  wASM('DecimalSep       db '',''');
  wASM('ThousandSep      db ''.''');
  wASM('Precision        dd 2');
  wASM('Digits           dd 2');
  wASM('conBuffer		     db 1,16,''                ''');

  wASM('parstr           db 127,0,127 dup(?)');
  wASM('str              db 255,0,255 dup(32)');
  wASM('concat           db 255,0,255 dup(0)');
  wASM('cpystr           db 255,0,255 dup(0)');
  wASM('nts              db 7,0,7 dup(0)');
  wASM('zero             db 0');
  wASM('dummystr         db 1,1,0');
  wASM('chrchr           db 1,1,32');
  wASM('minus            db 1,1,"-" ');

  wASM('Allocated        dd 0');
  wASM('MemBlock         dd 1024 dup (0); dynamischen Speicher reservieren');
  wASM('EndPtr           dd 0');

  wAsm(';======== global Vars');

  // Die STRINGKonstanten ausgeben
  For iVarIDX := 0 To oStrings.count - 1 Do
  Begin
    // die Strings sind mit Hochkomma!! Die werden nicht als Strings vom
    // ASSEMBLER verarbeitet... Daher "StringLänge - 2"
    wASM('$_S' + inttostr(iVarIDX + 1) + ' db ' +
      Inttostr(Length(oStrings.strings[iVarIDX]) - 2) +
      ',' + Inttostr(Length(oStrings.strings[iVarIDX]) - 2) +
      ',' + oStrings.strings[iVarIDX]
      );
  End;

  // Die Variablenliste...
  For iVarIDX := 0 To oVarList.count - 1 Do
  Begin
    L_VarEnter := TcVarEnter(oVarList.Items[iVarIDX]);
    If L_VarEnter.tpVisibility = tmGlobal Then
    Begin
      Case L_VarEnter.tpType Of
        tNone:
          Begin
          End;
        tBoolean:
          Begin
            wASM(L_VarEnter.sName + ' db 0');
          End;
        tByte:
          Begin
            wASM(L_VarEnter.sName + ' db 0');
          End;
        tChar:
          Begin
            wASM(L_VarEnter.sName + ' db 0');
          End;
        tU_Int16:
          Begin
            wASM(L_VarEnter.sName + ' dw 0');
          End;
        tU_Int32:
          Begin
            wASM(L_VarEnter.sName + ' dd 0');
          End;
        tInt16:
          Begin
            wASM(L_VarEnter.sName + ' dw 0');
          End;
        tInt32:
          Begin
            wASM(L_VarEnter.sName + ' dd 0');
          End;
        tPointer:
          Begin
            wASM(L_VarEnter.sName + ' dd 0');
          End;
        tSingle:
          Begin
            wASM(L_VarEnter.sName + ' dd 0');
          End;
        tString:
          Begin
            wASM(L_VarEnter.sName + ' db 0,0, 80 DUP(32)');
          End;
        tArray:
          Begin
            wASM(L_VarEnter.sName + ' db ' + Inttostr(L_VarEnter.iByteLen) + ' DUP(0)');
          End;
        tClass:
          Begin
            wASM(L_VarEnter.sName + ' dd 0,0,0,0');
          End;
        tFunction:
          Begin
          End;
        tProcedure:
          Begin
          End;
      End;
    End;
  End;
  wASM('ends');
  wASM('end start');
End;
{*******************************************************************************
Procedure:  Call_ProcedureParameters
Description: Parameterübergabe
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TCompiler.Call_ProcedureParameters(cVarEnter: TcVarEnter);
Var
  iParamCount       : word;
  iParamIDX         : Word;
  cParameter        : TcVarEnter;
  cExpressType      : TType;
  cParamType        : TType;
Begin
  iParamCount := 0;
  iParamIDX := cVarEnter.iIDX + 1;

  If tkToken = ttLB Then
  Begin
    NextToken;

    While Not bError And (tkToken <> ttRB) Do
    Begin
      cParameter := TcVarEnter(oVarList.Items[iParamIDX]);
      cParamType := cParameter.tpType;
      cExpressType := Call_Expression;

      cParamType := self.CastType(cParamType, cExpressType);

      // die Übergabe und der Parameter müssen übereinstimmen!
      If cParamType = tNone Then
        self.Error('Type mismatch');

      Case cParamType Of
        tNone:
          Begin
            self.Error('Type mismatch');
          End;
        tBoolean:
          Begin
            wAsm('push ah')
          End;
        tByte:
          Begin
            wAsm('push ah')
          End;
        tChar:
          Begin
            wAsm('push ah')
          End;
        tU_Int16:
          Begin
            wAsm('push ax')
          End;
        tU_Int32:
          Begin
            wAsm('push eax')
          End;
        tInt16:
          Begin
            wAsm('push ax')
          End;
        tInt32:
          Begin
            wAsm('push eax')
          End;
        tPointer:
          Begin
            wAsm('push eax')
          End;
        tSingle:
          Begin
            wAsm('push eax')
          End;
        tString:
          Begin
            wAsm('push eax')
          End;
        tArray:
          Begin
            wAsm('push eax')
          End;
        tClass:
          Begin
            wAsm('push eax')
          End;
        tFunction:
          Begin
            wAsm('push eax')
          End;
        tProcedure:
          Begin
            wAsm('push eax')
          End;
      End;

      If tkToken = ttCol Then
        NextToken;

      inc(iParamIDX);
    End;
  End;

End;
{*******************************************************************************
Procedure:  CastType
Description: Type Cast
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TCompiler.CastType(Type1, Type2: TType): TType;
Begin
  // wenn die Typen gleich sind, dann ist alles OK
  If Type1 = Type2 Then
    Result := Type1
  Else If (Type1 = tNone) Or (Type2 = TNone) Then
    Result := TNone
  Else
    // Casting
    Case Type1 Of
      tBoolean:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;

            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tByte:
        Begin
          Case Type2 Of
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;

      tChar:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tU_Int16:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tInt16:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TInt32;
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray
              :
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tInt32:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TInt32;
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tPointer:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TInt32;
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tSingle:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := tSingle
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tString:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tArray:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tClass:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tFunction:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tProcedure:
              Begin
                Result := TNone
              End;
          End;

        End;
      tProcedure:
        Begin
          Case Type2 Of
            tByte:
              Begin
                Result := TNone
              End;
            tChar:
              Begin
                Result := TNone
              End;
            tU_Int16:
              Begin
                Result := TNone
              End;
            tInt16:
              Begin
                Result := TNone
              End;
            tInt32:
              Begin
                Result := TNone
              End;
            tPointer:
              Begin
                Result := TNone
              End;
            tSingle:
              Begin
                Result := TNone
              End;
            tString:
              Begin
                Result := TNone
              End;
            tArray:
              Begin
                Result := TNone
              End;
            tClass:
              Begin
                Result := TNone
              End;
            tFunction:
              Begin
                Result := TNone
              End;
          End;

        End;
    End;
End;

End.

