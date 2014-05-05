unit FSVclBase;

interface

uses
  SysUtils, Classes, Windows, Controls;

type
  TMouseFlags = (mfMouseOver, mfLButtonDown, mfRButtonDown);
  TMouseFlag = set of TMouseFlags;

procedure GetMsgCursor(out pt: TPoint);

implementation

procedure GetMsgCursor(out pt: TPoint);
var
  combine: DWORD;
begin
  combine := GetMessagePos;
  pt.X := combine and $ffff;
  pt.Y := combine shr 16;
end;

end.
