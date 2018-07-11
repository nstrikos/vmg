unit postprocessdlg;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Spin, appsettings, constants;

type

  { TPostProcessDialog }

  TPostProcessDialog = class(TForm)
    constructor Create(AOwner: TComponent); override;
  private
    PageControl: TPageControl;
    TabInterpolation: TTabSheet;
    InterRadioGroup: TRadioGroup;
    SimpleInterRadio, CatMullRadio, PolyramaRadio : TRadioButton;
    TabRemap: TTabSheet;
    ChkUseRemap: TCheckBox;
    RemapWidthLabel, RemapHeightLabel: TLabel;
    RemapWidthEdit, RemapHeightEdit : TSpinEdit;
    AppSettings: TConfigurations;
    OkButton, CancelButton, ResetButton: TButton;
    procedure OkClicked(Sender: Tobject);
    procedure CancelClicked(Sender: TObject);
    procedure ResetClicked(Sender: TObject);
    procedure UpdateUseRemap(Sender: TObject);
  public
    procedure SetConfigurations(Settings: TConfigurations);
  end;

var
  vPostProcessDialogForm: TPostProcessDialog;

implementation


{ TPostProcessDialog }

constructor TPostProcessDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Position := poScreenCenter;
  Height := 400;
  Width := 500;
  Caption := 'Additional Process Dialog';


  PageControl := TPageControl.Create(Self);
  with PageControl do
  begin
    Parent := Self;
    Height := 275;
    Left := 25;
    Top := 25;
    Width := 450;
    TabOrder := 0;
    Anchors := [akLeft, akRight, akTop];
  end;

  TabInterpolation := TTabSheet.Create(Self);
  TabInterpolation.PageControl := PageControl;
  TabInterpolation.Caption := 'Anti-Aliasing';

  InterRadioGroup := TRadioGroup.Create(Self);
  InterRadioGroup.Parent := tabInterpolation;
  //interRadioGroup.Top := 50;
  InterRadioGroup.Caption := 'Choose method:';
  InterRadioGroup.Align := alClient;

  SimpleInterRadio := TRadioButton.Create(interRadioGroup);
  SimpleInterRadio.Caption := 'Simple';

  CatMullRadio := TRadioButton.Create(interRadioGroup);
  CatMullRadio.Caption := 'CatMull';

  PolyramaRadio := TRadioButton.Create(interRadioGroup);
  PolyramaRadio.Caption := 'Polyrama';

  InterRadioGroup.Items.AddObject('Simple', SimpleInterRadio);
  InterRadioGroup.Items.AddObject('Polyrama', PolyramaRadio);
  InterRadioGroup.Items.AddObject('CatMull', catMullRadio);
  InterRadioGroup.ItemIndex := 0;

  TabRemap := TTabSheet.Create(Self);
  TabRemap.PageControl := PageControl;
  TabRemap.Caption := 'Remapping';

  ChkUseRemap := TCheckBox.Create(Self);
  with ChkUseRemap do
  begin
    Parent := TabRemap;
    Caption := 'Enable remapping';
    Top := 10;
    Left := 10;
    Width := 400;
    OnClick := @UpdateUseRemap;
  end;

  RemapWidthLabel := TLabel.Create(Self);
  with RemapWidthLabel do
  begin
    Parent := TabRemap;
    Caption := 'Width:';
    Top := 60;
    Left := 10;
    Width := 50;
  end;

  RemapWidthEdit := TSpinEdit.Create(Self);
  with RemapWidthEdit do
  begin
    Parent := TabRemap;
    Top := 60 - RemapWidthLabel.Height div 2;
    Left := RemapWidthLabel.Left + RemapWidthLabel.Width + 10;
    Width := 100;
    MinValue := 1;
    MaxValue := 200;
    Value := 50;
  end;

  RemapHeightLabel := TLabel.Create(Self);
  with RemapHeightLabel do
  begin
    Parent := TabRemap;
    Caption := 'Height:';
    Top := 100;
    Left := 10;
    Width := 50;
  end;

  RemapHeightEdit := TSpinEdit.Create(Self);
  with RemapHeightEdit do
  begin
    Parent := TabRemap;
    Top := RemapHeightLabel.Top - RemapHeightLabel.Height div 2;
    Left := RemapHeightLabel.Left + RemapHeightLabel.Width + 10;
    Width := 100;
    MinValue := 1;
    MaxValue := 200;
    Value := 50;
  end;

  CancelButton := TButton.Create(Self);
  with CancelButton do
  begin
    Parent := Self;
    Top := 320;
    Left := 255;
    Width := 100;
    Height := 40;
    Caption := 'Cancel';
    OnClick := @CancelClicked;
  end;

  OkButton := TButton.Create(Self);
  with OkButton do
  begin
    Parent := Self;
    Top := 320;
    Left := CancelButton.Left + CancelButton.Width + 20; ;
    Width := 100;
    Height := 40;
    Caption := 'OK';
    OnClick := @OkClicked;
  end;

  ResetButton := TButton.Create(Self);
  with ResetButton do
  begin
    Parent := tabRemap;
    Top := 200;
    Left := RemapWidthLabel.Left;
    Height := 40;
    Width := RemapWidthLabel.Width + RemapWidthEdit.Width + 10;
    Caption := 'Reset to defaults';
    OnClick := @ResetClicked;
  end;

end;

procedure TPostProcessDialog.OkClicked(Sender: Tobject);
begin
     AppSettings.UseRemap := chkUseRemap.Checked;
     AppSettings.RemapWidth := RemapWidthEdit.Value;
     AppSettings.RemapHeight := RemapHeightEdit.Value;
     AppSettings.AntiAliasingMode := InterRadioGroup.Items[InterRadioGroup.ItemIndex];
     ModalResult := mrOK;
end;

procedure TPostProcessDialog.CancelClicked(Sender: TObject);
begin
     ModalResult := mrCancel;
end;

procedure TPostProcessDialog.ResetClicked(Sender: TObject);
begin
     RemapWidthEdit.Value := 50;
     RemapHeightEdit.Value := 50;
end;

procedure TPostProcessDialog.UpdateUseRemap(Sender: TObject);
begin
    RemapWidthEdit.Enabled := chkUseRemap.Checked;
    RemapHeightEdit.Enabled := chkUseRemap.Checked;
end;

procedure TPostProcessDialog.setConfigurations(Settings: TConfigurations);
begin
     appSettings := Settings;
     chkUseRemap.Checked := appSettings.UseRemap;
     RemapWidthEdit.Value := appSettings.RemapWidth;
     RemapHeightEdit.Value := appSettings.RemapHeight;
     UpdateUseRemap(Self);

     if (AppSettings.AntiAliasingMode =  IdentSimpleAntiAliasing) then
        InterRadioGroup.ItemIndex := 0
     else if AppSettings.AntiAliasingMode = IdentCatMullAntiAliasing then
        InterRadioGroup.ItemIndex := 1
     else if AppSettings.AntiAliasingMode = IdentPolyramaAntiAliasing then
        InterRadioGroup.ItemIndex := 2;
end;

end.

