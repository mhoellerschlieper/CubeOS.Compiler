Unit uCubeOOP_Globals;

Interface
Uses
  SysUtils,
  Classes,
  uCubeOOP_Parser;

Type
  tType =
    (tNone,
    tBoolean,
    tByte,
    tChar,
    tU_Int16,
    tU_Int32,
    tInt16,
    tInt32,
    tPointer,
    tSingle,
    tString,
    tArray,
    tClass,
    tFunction,
    tProcedure
    );

  tVisibility =
    (
    tmNone,
    tmGlobal,
    tmConstant,
    tmMember,
    tmLocal,
    tmType,
    tmVarParameter,
    tmParameter
    );

  tKind =
    (tkNone,
    tkPublic,
    tkPrivate
    );
  TConvert32Bit = (Conv_AsSingle, Conv_AsInteger, Conv_AsByte, Conv_AsChar, Conv_AsCardinal);

  // für die Konvertierung von Dezimalzahlen in BCD - Zahlen
  TSingleToHex = Record
    Case TConvert32Bit Of
      Conv_AsSingle: (SingleValue: Single);
      Conv_AsInteger: (IntValue: integer);
      Conv_AsByte: (ByteHH, ByteHL, ByteLH, ByteLL: Byte);
      Conv_AsChar: (CharHH, CharHL, CharLH, CharLL: Char);
      Conv_AsCardinal: (CardinalValue: Cardinal);
  End;

  TRealToHex = Record
    Case Boolean Of
      True: (ValFlt: Real);
      False: (ValInt: Int64);
  End;

  TDoubleToHex = Record
    Case Boolean Of
      True: (ValFlt: Double);
      False: (ValInt: Int64);
  End;

Const

  cTypeText         : Array[tType] Of String =
    ('tNone',
    'tBoolean',
    'tByte',
    'tChar',
    'tU_Int16',
    'tU_Int32',
    'tInt16',
    'tInt32',
    'tPointer',
    'tFloat',
    'tString',
    'tArray',
    'tClass',
    'tFunction',
    'tProcedure'
    );

  cTypeTextFull         : Array[tType] Of String =
    ('none',
    'boolean',
    'byte',
    'char',
    'u_Int16',
    'u_Int32',
    'int16',
    'int32',
    'pointer',
    'single',
    'string',
    'array',
    'class',
    'function',
    'procedure'
    );

  cVisiblyText      : Array[tVisibility] Of String =
    (
    'tmNone',
    'tmGlobal',
    'tmConstant',
    'tmMember',
    'tmLocal',
    'tmType',
    'tmVarParameter',
    'tmParameter'

    );

  cKindText         : Array[tKind] Of String =
    ('tkNone',
    'tkPublic',
    'tkPrivate'
    );

  cByteLen          : Array[tType] Of Byte =
    (0,                                 //tNone
    1,                                  //tBoolean
    1,                                  //tByte
    1,                                  //tChar
    2,                                  //tU_Int16
    4,                                  //tU_Int32
    2,                                  //tInt
    4,                                  //tInt32
    4,                                  //tPointer
    4,                                  //tSingle
    4,                                  //tString
    4,                                  //tArray
    4,                                  //tClass
    4,                                  //tFunction
    4);                                 //tProcedure

Type
  TInfoStruct = Record
    iLinesCompiled: INteger;
    iErrorLine: Integer;
    sErrorText: String;
    VarList: TList;
  End;

  TArrayDim = Record
    iArrayFrom: Integer;
    iArrayTo: Integer;
  End;

  TDescriptor = Record
    sClass: String;
    sName: String;
    sProcedure: String;
    sClassType: String;
    tpType: TType;
    tpVisibility: tVisibility;
    tkKind: TKind;                      // Public, Private
    sConstantValue: String;
    Resulttype: TType;                  // Rückgabeergebnis einer Funktion
    iArrayDim: Integer;
    iArrayElements: Integer;
    iArrayDims: Array Of TArrayDim;
    ArrayTyp: TType;
  End;

  TcVarEnter = Class
    iIDX: Integer;
    sClass: String;
    sName: String;
    sProcedure: String;
    sClassType: String;
    tpType: TType;
    tpVisibility: tVisibility;
    tkKind: TKind;                      // Public, Private
    iByteLen: Byte;
    iOffset: Integer;
    iStackSize: Integer;
    iLocalStack: Integer;
    sConstantValue: String;
    Resulttype: TType;                  // Rückgabeergebnis einer Funktion

    // Feldorganisation :-) Arrays und Strings
    iArrayDim: Integer;
    iArrayElements: Integer;
    iArrayDims: Array Of TArrayDim;
    ArrayTyp: TType;
  End;

  TVarList = Class(TList)

    Procedure Add(Descriptor: TDescriptor);
    Function Find(sName: String): TcVarEnter;
    Function FindClass(sClassName: String): TcVarEnter;
    Function FindClassIdent(sName, sClassName: String): TcVarEnter;

    Procedure CalcClassOffsets(sClassName: String);
  End;

Function TokenToType(TKToken: TTokenKind): TType;
Function StringToType(TKToken: String): TType;

Function DoubleToHex(Value: Double): String;
Function SingleToHex(Value: Single): String;

Implementation

{ TVarList }
{*******************************************************************************
Procedure:
Description: Beschreibung eines/r Objektes, Variable, Prozedur....
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TVarList.Add(Descriptor: TDescriptor);
Var
  L_VarEnter        : TcVarEnter;
  iArrayIDX         : Integer;
Begin
  L_VarEnter := TcVarEnter.Create;

  L_VarEnter.iIDX := Count;             // Die Position merken
  L_VarEnter.sClass := Descriptor.sClass;
  L_VarEnter.sName := Descriptor.sName;
  L_VarEnter.sClassType := Descriptor.sClassType;
  L_VarEnter.sProcedure := Descriptor.sProcedure;
  L_VarEnter.tpType := Descriptor.tpType;
  L_VarEnter.tpVisibility := Descriptor.tpVisibility;
  L_VarEnter.tkKind := Descriptor.tkKind;

  // berechne für String und Array die Anzahl der zu reservierenden Bytes...
  If Descriptor.tpType In [tArray, tString] Then
    L_VarEnter.iByteLen := Descriptor.iArrayElements * cByteLen[Descriptor.ArrayTyp]
  Else
    L_VarEnter.iByteLen := cByteLen[Descriptor.tpType];

  L_VarEnter.sConstantValue := Descriptor.sConstantValue;
  L_VarEnter.Resulttype := Descriptor.Resulttype;

  // kopiere für ARRAY und String die Felddimensionen...
  L_VarEnter.iArrayDim := Descriptor.iArrayDim;
  L_VarEnter.iArrayElements := Descriptor.iArrayElements;
  L_VarEnter.ArrayTyp := Descriptor.ArrayTyp;

  SetLength(L_VarEnter.iArrayDims, High(Descriptor.iArrayDims) + 1);

  For iArrayIDX := 0 To High(Descriptor.iArrayDims) Do
    L_VarEnter.iArrayDims[iArrayIDX] := Descriptor.iArrayDims[iArrayIDX];

  Inherited Add(L_VarEnter);            // Objekt in Liste hinzufügen

  CalcClassOffsets(Descriptor.sClass);  // berechne die Offset Adressen
End;
{*******************************************************************************
Procedure:
Description: berechne die Offsetadressen der Member und lokalen Variablen
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Procedure TVarList.CalcClassOffsets(sClassName: String);
Const
  c_iBaseOffsetParam = 8;
  c_iBaseOffsetLocal = 4;

Var
  iOffset           : Integer;
  iLocalOffset      : Integer;
  iVarListIDX       : Integer;
  tpVisibility      : tVisibility;
  Variable          : TcVarEnter;
  iStackSize        : Integer;
  ResultType        : TType;
Begin
  Try
    iOffset := 0;
    iLocalOffset := 0;

    If Count > 0 Then
    Begin

      tpVisibility := TcVarEnter(Items[count - 1]).tpVisibility;

      iVarListIDX := count - 1;
      Variable := TcVarEnter(Items[iVarListIDX]);

      While iVarListIDX > 0 Do
      Begin
        // globale Varlablen besitzen keinen OFFSET
        While (Variable.tpVisibility = tmGlobal) And (iVarListIDX > 0) Do
        Begin
          Dec(iVarListIDX);

          If iVarListIDX >= 0 Then
            Variable := TcVarEnter(Items[iVarListIDX]);
        End;

        ResultType := tNone;
        If (Variable.sClass = sClassName) Then
        Begin

          // Lokale Offsets Summieren
          iLocalOffset := 0;
          While (Variable.tpVisibility = tmLocal) And (iVarListIDX > 0) Do
          Begin
            If Lowercase(Variable.sName) = 'result' Then
              ResultType := Variable.tpType;

            Variable.iOffset := iLocalOffset + c_iBaseOffsetLocal;
            iLocalOffset := iLocalOffset + Variable.iByteLen;
            Dec(iVarListIDX);
            If iVarListIDX >= 0 Then
              Variable := TcVarEnter(Items[iVarListIDX]);

          End;

          iStackSize := 0;
          // Prozedurparameter aufsummieren
          While (Variable.tpVisibility In [tmVarParameter, tmParameter]) And (iVarListIDX > 0) Do
          Begin
            Variable.iOffset := iStackSize + c_iBaseOffsetParam;
            iStackSize := iStackSize + Variable.iByteLen;
            Dec(iVarListIDX);
            If iVarListIDX >= 0 Then
              Variable := TcVarEnter(Items[iVarListIDX]);
          End;

          // Prozedurparameter Stackinfos zuweisen
          If Variable.tpType In [tProcedure, tFunction] Then
          Begin
            If iLocalOffset > 0 Then
              Variable.iLocalStack := iLocalOffset {  + c_iBaseOffsetLocal}
            Else
              Variable.iLocalStack := 0;

            If iStackSize > 0 Then
              Variable.iStackSize := iStackSize {+ c_iBaseOffsetParam}
            Else
              Variable.iStackSize := 0;

            Variable.Resulttype := ResultType;

            Dec(iVarListIDX);
            If iVarListIDX >= 0 Then
              Variable := TcVarEnter(Items[iVarListIDX]);
          End;

          iOffset := 0;
          While (Variable.tpVisibility = tmMember) And
            (Variable.tpType <> tProcedure) And
            (Variable.tpType <> tFunction)
            And (iVarListIDX > 0) Do
          Begin
            Variable.iOffset := iOffset;
            iOffset := iOffset + Variable.iByteLen;
            Dec(iVarListIDX);

            If iVarListIDX >= 0 Then
              Variable := TcVarEnter(Items[iVarListIDX]);
          End;

          If Variable.tpType = tClass Then
          Begin
            Variable.iOffset := iOffset;
            Dec(iVarListIDX);
            If iVarListIDX >= 0 Then
              Variable := TcVarEnter(Items[iVarListIDX]);
          End
        End
        Else
        Begin
          Dec(iVarListIDX);
          If iVarListIDX >= 0 Then
            Variable := TcVarEnter(Items[iVarListIDX]);
        End
      End;
    End;

  Except
  End;
End;
{*******************************************************************************
Procedure:
Description: Finde eine Variable
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TVarList.Find(sName: String): TcVarEnter;
Var
  iVarListIDX       : Integer;
Begin
  Result := Nil;
  // suche in der Liste nach Namen
  For iVarListIDX := {0 to } Count - 1 Downto 0 Do // Änderung MH 22.06.2009
    If (Lowercase(TcVarEnter(Items[iVarListIDX]).sName) = Lowercase(sName)) Then
    Begin
      Result := TcVarEnter(Items[iVarListIDX]);
      Break;
    End;
End;
{*******************************************************************************
Procedure:
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TVarList.FindClassIdent(sName, sClassName: String): TcVarEnter;
Var
  iVarListIDX       : Integer;
Begin
  Result := Nil;
  // suche in der Liste nach Namen
  For iVarListIDX := {0 to } Count - 1 Downto 0 Do // Änderung MH 22.06.2009
    If (Lowercase(TcVarEnter(Items[iVarListIDX]).sName) = Lowercase(sName)) And
      (Lowercase(TcVarEnter(Items[iVarListIDX]).sClass) = Lowercase(sClassName)) Then
    Begin
      Result := TcVarEnter(Items[iVarListIDX]);
      Break;
    End;
End;
{*******************************************************************************
Procedure:
Description:
Author: MHS
Date:   29.07.2007
History: V1.0
*******************************************************************************}

Function TVarList.FindClass(sClassName: String): TcVarEnter;
Var
  iVarListIDX       : Integer;
Begin
  Result := Nil;
  // suche in der Liste nach Namen
  For iVarListIDX := Count - 1 Downto 0 Do
    If (Lowercase(TcVarEnter(Items[iVarListIDX]).sClass) = Lowercase(sClassName))
      And
      (TcVarEnter(Items[iVarListIDX]).tpType = tClass) And
      (TcVarEnter(Items[iVarListIDX]).sClassType = '') Then
    Begin
      Result := TcVarEnter(Items[iVarListIDX]);
      Break;
    End;
End;

{
'*******************************************************************************
* Modul :  RealToHex
* Desc  :  Konvertierungsroutinen, um die Float-Konstanten in den Quellen umzuwandeln in ein BCD - Binärformat
* Author:  M.Höller-Schlieper
* ------------------------------------------------------------------------------
* History:
*          28.01.2014 MH Creation
'*******************************************************************************
}

Function DoubleToHex(Value: Double): String;
Var
  DoubleVal         : TDoubleToHex;

Begin
  DoubleVal.ValFlt := Value;
  Result := IntToHex(DoubleVal.ValInt, 8) + 'h';
End;

Function SingleToHex(Value: Single): String;
Var
  SingleVal         : TSingleToHex;
  s1                : Single;
  s                 : String;
Begin
  SingleVal.SingleValue := Value;
  Result := IntToHex(SingleVal.IntValue, 8) + 'h'; //'0x' + IntToHex(SingleVal.ValInt, 16);
End;

Function TokenToType(TKToken: TTokenKind): TType;
Begin
  Case TKToken Of
    ttIdentifier:                       // Datentyp ist eine ander Klasse
      Begin
        Result := tClass;
      End;
    ttBoolean:
      Begin
        Result := tBoolean;
      End;
    ttByte:
      Begin
        Result := tByte;
      End;
    ttChar:
      Begin
        Result := tChar;
      End;
    ttU_Int16:
      Begin
        Result := tU_Int16;
      End;

    ttU_Int32:
      Begin
        Result := tU_Int32;
      End;
    ttInt16:
      Begin
        Result := tInt16;
      End;
    ttInt32:
      Begin
        Result := tInt32;
      End;
    ttPointer:
      Begin
        Result := tPointer;
      End;
    ttSingle:
      Begin
        Result := tSingle;
      End;
    ttString:
      Begin
        Result := tString;
      End;
    ttArray:
      Begin
        Result := tArray;
      End;
  End;

End;

Function StringToType(TKToken: String): TType;
Var
  i                 : tType;
Begin
  For i := tNone To tProcedure Do
    If lowercase(TKToken) = cTypeTextFull[i] Then
    Begin
      result := i;
      break
    End;

End;

End.

