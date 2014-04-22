unit FSVclRegister;

interface

uses
  SysUtils, Classes, Windows, DesignEditors, DesignIntf, DesignMenus, ComponentDesigner,
  FSTheme, FSGraphics, FSControls, FSStdCtrls, FSSkinCtrls, FSScrollControls, FsNavTree;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('FSVCL', [TFsTheme, TFsRectangle, TFsNinePitchDrawable, TFsPictureDrawable,
    TFsGradientDrawer, TFsImage, TFsImageButton, TFsCoverButton, TFsCheckBox,
    TFsSkinButton, TFsSkinCheckBox,
    TFsPanel, TFsPageControl, TFsEdit, TFsButtonEdit, TFsFlatScrollBar,
    TFsMemo, TFsListBox, TFsListView, TFsTreeView, TFsNavTree]);
end;

end.
