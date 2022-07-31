unit MemoUtils;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
    Forms, Menus, Dialogs, StdCtrls, ComCtrls, ActnList,
    Controls, ExtCtrls, Classes, fgl, SysUtils;

type

  TMemoHistoryStep = record
    dateTime: TDateTime;
    Text: String;
    CaretPos: TPoint; // used to check if the caret has been moved
    SelStart: Integer;
    CumulatedTextSize:Integer; // used to display undo buffer memory size
    class operator = (a,b:TMemoHistoryStep):Boolean;
  end;

  TMemoHistory = specialize TFPGList<TMemoHistoryStep>;

  { TMemoUndo }

  TMemoUndo = class(TObject)
	private
        _isInternalChange:boolean;
        procedure _pushStep(step:TMemoHistoryStep);
	protected
		chg: record
			keyPressed:integer;
			sepPressed:integer;
		end;
        FTextSinceLastSave:string;
		FHistory:TMemoHistory;
		FCurrentHistoryIndex:Integer;
        FNotifyHistoryChanged:TNotifyEvent;
        FCurrStep:TMemoHistoryStep;
        FPaused:boolean;
        function stepNow:TMemoHistoryStep;
    public
		Memo:TMemo;
		procedure OnMemoChange();
		procedure OnIdle();
		procedure OnKeyPress(Key:Char);
		procedure Undo;
		procedure Redo;
		procedure Reset();
		procedure SaveForUndo(force:boolean = false);
		constructor Create;
		destructor Destroy(); override;
	published
		property History:TMemoHistory read FHistory;
        property CurrentHistoryIndex:integer read FCurrentHistoryIndex;
        property OnHistoryChanged:TNotifyEvent read FNotifyHistoryChanged write FNotifyHistoryChanged;
        property Paused:boolean read FPaused write FPaused;
  end;

implementation

uses
    LazLogger;

class operator TMemoHistoryStep.= (a,b: TMemoHistoryStep) Result: Boolean;
begin
  Result:=a.dateTime=b.dateTime;
end;

function TMemoUndo.stepNow:TMemoHistoryStep;
begin
  result.dateTime:=Now;
  result.Text:= Memo.Text;
  result.CaretPos:= Memo.CaretPos;
  result.SelStart:= Memo.SelStart;
  result.CumulatedTextSize:= 0;
end;

procedure dumpHistory(hist:TMemoHistory);
var i:integer;
begin
 for i:=0 to hist.Count-1 do begin
 	debugln('   ' + intToStr(i) + ':' + intToStr(hist.Items[i].SelStart)
    + ' ->' + stringReplace(hist.Items[i].Text, #13#10, '¤', [rfReplaceAll]));
     end;
end;

{ TMemoUndo }

var lastSelStart:integer = 0;

constructor TMemoUndo.Create;
begin
    FHistory := TMemoHistory.Create;
    _isInternalChange := false;
    FPaused := false;
    inherited Create;
end;

destructor TMemoUndo.Destroy;
begin
 	FreeAndNil(FHistory);
    Memo := nil;
	inherited Destroy;
end;

procedure TMemoUndo.OnIdle;
var step:TMemoHistoryStep;
begin
 if FPaused then exit;
 // save if there are unsaved data and caretpos is < last pos
 if (chg.keyPressed > 0) and (abs(memo.selStart - lastSelStart) > 1) then
 //debugln('should save now !');
 lastSelStart := memo.selStart;
  // keep last caret pos
  if (FHistory.Count > 0) and (chg.keyPressed = 0)
  and (FHistory.Last.CaretPos <> Memo.CaretPos)
  and (FCurrentHistoryIndex = FHistory.Count-1) then begin
       step := FHistory.Last;
       step.CaretPos := Memo.CaretPos;
       step.SelStart:=Memo.selStart;
       FHistory.Last := step;
       (*
       DebugLn('saving caret, selStart=' + IntToStr(memo.selStart)
       + ', selLen=' + intToStr(memo.selLength)
       + ', caretPos.x=' + intToStr(memo.caretPos.X)
       );
       *)
       //dumpHistory(history);
       //debugln('    History.Last.Text=' + History.Last.Text);

//       UpdateStatusBar();
	end;
   //DebugLn('idle');
end;

procedure TMemoUndo.OnKeyPress(Key: Char);
begin
  if FPaused then exit;
 //debugln('kp=' + Key + ' ('+IntToStr(Byte(key))+')');
     chg.keyPressed += 1;
     if (Key = ' ') or (Key = #13) or (Key = #10) or (Key = #9) then
     	chg.sepPressed += 1;
  //debugln('    keyPress=' + inttostr(chg.keyPressed) + #9 + 'sep=' + inttostr(chg.sepPressed));
end;

var lastKeyPressed:integer=0;
procedure TMemoUndo.OnMemoChange;
begin
     if FPaused then exit;
  //debugln('memoChange: key=' + IntToStr(chg.keyPressed) + ' sep=' + IntToStr(chg.sepPressed));
	if _isInternalChange then exit;

 	// CTRL+X or CTRL+V or menu click - flow is broken
 	if (chg.keyPressed = 0) or (chg.keyPressed = lastKeyPressed) then begin
         //debugln('keyPressed=0 => pushing CurrStep, then saveForUndo');
         // add last situation
		_pushStep(FCurrStep);
        saveForUndo();
     end
    // in flow, new word
    else if (chg.sepPressed > 0) then begin
        //debugln('sepPressed>0 => saveForUndo');
     	SaveForUndo();
	end;

    lastKeyPressed := chg.keyPressed;
    FCurrStep := stepNow();
end;

procedure TMemoUndo.Redo;
begin
 debugln('redo');
  // no available step
  if (FCurrentHistoryIndex >= FHistory.Count - 1) then exit;
  // proceed
  _isInternalChange := true;
 FCurrentHistoryIndex += 1;
 Memo.Text := FHistory.Items[FCurrentHistoryIndex].Text;
 Memo.CaretPos := FHistory.Items[FCurrentHistoryIndex].CaretPos;
 Memo.SelStart:= FHistory.Items[FCurrentHistoryIndex].SelStart;
 _isInternalChange := false;
 //UpdateStatusBar();

 //debugln('redo selstart=' + IntToStr(FHistory.Items[FCurrentHistoryIndex].SelStart)
 //	+ #9 + ' caretPos=' + IntToStr(FHistory.Items[FCurrentHistoryIndex].CaretPos.X));
chg.keyPressed:=0;
chg.sepPressed:=0;

  if assigned(FNotifyHistoryChanged) then
   FNotifyHistoryChanged(self);
end;

procedure TMemoUndo.Reset;
begin
  FHistory.Clear();
  FCurrentHistoryIndex := 0;
  SaveForUndo();
end;

procedure TMemoUndo._pushStep(Step:TMemoHistoryStep);
begin
	// delete forward undo's if any
	while (FCurrentHistoryIndex < FHistory.Count - 1) do
		History.Delete(FHistory.Count - 1);
    // update CumulatedTextSize
	step.CumulatedTextSize := Length(step.Text);
	if (FHistory.Count > 0) then
		step.CumulatedTextSize += FHistory.Last.CumulatedTextSize;
    // push and update index
	FHistory.Add(step);
	FCurrentHistoryIndex := FHistory.Count-1;
  // notify ?
  //if assigned(FNotifyHistoryChanged) then
    //FNotifyHistoryChanged(self);
end;

procedure TMemoUndo.SaveForUndo(force:boolean);
var len: Integer = 0;
  step: TMemoHistoryStep;
begin
  len := FHistory.Count;
  //debugln('saving for undo, len=' + IntToStr(len) + ' historyIndex=' + IntToStr(FCurrentHistoryIndex));
  if force or (len = 0) or (
  (len > 0) and (Length(FHistory[FCurrentHistoryIndex].Text) <> Length(Memo.Text))
  ) then begin
     // delete forward undo's if any
     while (FCurrentHistoryIndex < FHistory.Count - 1) do
           History.Delete(FHistory.Count - 1);
     len:=FHistory.Count;
     // create and push new step
     step.dateTime:=Now;
     step.text := Memo.Text;
     step.CaretPos := Memo.CaretPos;
     step.SelStart := Memo.SelStart;
     //debugln('   saved selStart=' + IntTostr(Memo.selstart) + ', caretPos.x=' + inttostr(memo.caretPos.X));
     //step.SelLen := Memo1.SelLength;
     step.CumulatedTextSize:=Length(step.Text);
     if (len > 0) then
        step.CumulatedTextSize += FHistory.Last.CumulatedTextSize;
     FHistory.Add(step);
     //dumpHistory(FHistory);
     // update currentHistoryIndex
     FCurrentHistoryIndex:=FHistory.Count-1;
     // reset currStep
     FCurrStep := stepNow();
     // notify
     if assigned(FNotifyHistoryChanged) then
	   FNotifyHistoryChanged(self);

  end;
  chg.keyPressed:=0;
  chg.sepPressed:=0;
end;

procedure TMemoUndo.Undo;
var len: Integer;
begin
  DebugLn('undo');

  if chg.keyPressed > 0 then
  	saveForUndo();

  // already at the beginning ?
  if (FCurrentHistoryIndex <= 0) then exit;

  // proceed
  _isInternalChange := true;
  FCurrentHistoryIndex -= 1;
  len := FHistory.Count;
  if (FCurrentHistoryIndex >= len) then
     FCurrentHistoryIndex := len-1;
  Memo.Text := FHistory.Items[FCurrentHistoryIndex].Text;
  //Memo1.CaretPos := History.Items[CurrentHistoryIndex].CaretPos;
  Memo.SelStart:=FHistory.Items[FCurrentHistoryIndex].selStart;
  _isInternalChange := false;
  //UpdateStatusBar();
  chg.keyPressed:=0;
  chg.sepPressed:=0;

  if assigned(FNotifyHistoryChanged) then
   FNotifyHistoryChanged(self);
end;

end.

