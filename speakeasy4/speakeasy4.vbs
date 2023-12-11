
'********************************************************************************
'*                                                                              *
'*                        Bally SPEAKEASY (1982)                                *
'*                                                                              *
'********************************************************************************


' VPX by mistermixer - Bord 
' primitives by Bord
' roulette script by Rothbauerw
' cards script by mfuegemann
' LUTs by Flupper1 

' Special Thanx to Bord,Rothbauerw,mfuegemann,JPSalas,Schreibi34,Flupper1,Arngrim
' and all table developers , scriptdoctors, blenderexperts, table tweakers ...


' script based on Bally's Lost World by Bord and vp9 version by JPSalas



Option Explicit
Randomize

Dim Ballsize,BallMass
BallSize = 49
BallMass = 1.45

Dim luts, lutpos
luts = array("LUT1", "LUT2", "LUT3", "LUT4", "LUT5","LUT6","LUT7", "LUT8", "LUT9", "LUT10", "LUT11", "LUT12", "LUT13", "LUT14", "LUT15", "LUT16", "LUT17", "LUT18", "LUT19", "LUT20", "LUT21", "LUT22","LUT23" )
lutpos = 5							'  set the nr of the LUT you want to use (0 = first in the list above, 1 = second, etc); 5 is the default
Const EnableRightMagnasave = 1		' 1 - on; 0 - off; if on then the right magnasave button let's you rotate all LUT's



Const VolDiv = 8000    ' Lower numper louder ballrolling sound
Const VolCol    = 3    ' Ball collision divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 8    ' Bumpers volume.
Const VolRol    = 25    ' Rollovers volume.
Const VolGates  = 5    ' Gates volume.
Const VolMetal  = 30    ' Metals volume.
Const VolRB     = 5    ' Rubber bands volume.
Const VolRH     = 5    ' Rubber hits volume.
Const VolPo     = 50   ' Rubber posts volume.
Const VolPi     = 5    ' Rubber pins volume.
Const VolPlast  = 30    ' Plastics volume.
Const VolTarg   = 30    ' Targets volume.
Const VolWood   = 20   ' Woods volume.
Const VolKick   = 1    ' Kicker volume.
Const VolSpin   = .1  ' Spinners volume.
Const VolFlip   = 5    ' Flipper volume.

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const cGameName    = "speakes4" ' PinMAME short game name

LoadVPM "01550000", "Bally.vbs", 3.26

Const UseSolenoids=2
Const UseLamps=1
Const UseGI=0


' Standard Sounds
Const SSolenoidOn="fx2_solenoid",SSolenoidOff="fx_solenoidoff",SCoin="fx_coin",SFlipperOn="",SFlipperOff=""

Dim bsTrough,bsLSaucer,bsRSaucer,HiddenValue



'*********** Desktop/Cabinet settings ************************

If Table1.ShowDT = true Then
	HiddenValue = 0
	ramp15.visible =1
    ramp16.visible=1
   
    
Else
	HiddenValue = 1
	ramp15.visible =0
    ramp16.visible=0

End If

'******************************************************
'						FLIPPERS
'******************************************************

Sub SolLFlipper(Enabled)
	If Enabled Then
		PlaySound SoundFX("fx_Flipperup",DOFContactors), VolFlip
		lf.fire
	Else
		if leftflipper.currentangle < leftflipper.startangle - 5 then 
			PlaySound SoundFX("fx_flipperdown",DOFContactors), VolFlip
		end if		
		LeftFlipper.RotateToStart
	End If
End Sub
 
Sub SolRFlipper(Enabled)
	If Enabled Then
		PlaySound SoundFX("fx_Flipperup1",DOFContactors), VolFlip
		RF.fire
	Else
		if RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			PlaySound SoundFX("fx_Flipperdown1",DOFContactors), VolFlip
		End If
		RightFlipper.RotateToStart
	End If
End Sub

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricksL LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricksR RightFlipper, RFPress, RFCount, RFEndAngle, RFState
Prim2.Rotx = Gate.Currentangle
Prim3.Rotx = Gate003.Currentangle
end sub

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
Const EOSTnew = 1.5 'FEOST
Const EOSAnew = 0.2
Const EOSRampup = 1.5 
Const SOSRampup = 8.5 
Const LiveCatch = 8

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperTricksR (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	If Flipper.currentangle < Flipper.startangle + 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle + 3
			Flipper.Elasticity = FElasticity
			FCount = 0 
			FState = 1
		End If
	ElseIf Flipper.currentangle = Flipper.endangle and FlipperPress = 1 then
		If FState <> 2  and FState Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			FState = 2
		End If

		if FCount = 0 Then FCount = GameTime

		if GameTime - FCount < LiveCatch Then
			Flipper.Elasticity = 0.1
			If Flipper.endangle <> FEndAngle Then Flipper.endangle = FEndAngle
		Else
			Flipper.Elasticity = FElasticity
		end if
	Elseif Flipper.currentangle < Flipper.endangle - 0.01 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Sub FlipperTricksL (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	If Flipper.currentangle > Flipper.startangle - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle + 3
			Flipper.Elasticity = FElasticity
			FCount = 0 
			FState = 1
		End If
	Elseif Flipper.currentangle = Flipper.endangle and FlipperPress = 1 then
		If FState <> 2  and FState Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			FState = 2
		End If

		if FCount = 0 Then FCount = GameTime

		if GameTime - FCount < LiveCatch Then
			Flipper.Elasticity = 0.1
			If Flipper.endangle <> FEndAngle Then Flipper.endangle = FEndAngle
		Else
			Flipper.Elasticity = FElasticity
		end if
	Elseif Flipper.currentangle > Flipper.endangle + 0.01 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub


'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(6) = "Solknocker"
SolCallback(7) = "bsTrough.SolOut"
SolCallBack(14) = "bsLSaucer.SolOut"
SolCallback(15) = "bsRSaucer.SolOut"
SolCallback(13) = "SolCardTargetsReset"
SolCallback(19) = "vpmNudge.SolGameOn"


SolCallback(sLLFlipper)="SolLFlipper"
SolCallback(sLRFlipper)="SolRFlipper"

'******************************************************
' 					TABLE INIT
'******************************************************

Dim dtDrop, ii
Dim ttGopherWheel

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Speakeasy"&chr(13)&"(Bally 1982)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .Hidden = 0
        .Run
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With

	PinmameTimer.Interval=PinMameInterval
	PinmameTimer.Enabled=1


	Set ttGopherWheel = New cvpmTurntable
		ttGopherWheel.InitTurntable ttGopherWheelTrig, 20 '20
		ttGopherWheel.spinup=100
		ttGopherWheel.spindown=100
		ttGopherWheel.CreateEvents "ttGopherWheel"

    Init_Roulette

	Set bsTrough=New cvpmBallstack
	with bsTrough
		.InitSw 0,25,0,0,0,0,0,0
		.InitNoTrough BallRelease,25,125,5
		.InitKick BallRelease,90,7
		.InitExitSnd Soundfx("fx2_ballrel",DOFContactors), Soundfx("fx2_solenoid",DOFContactors)
		.Balls=1
	end with
'
	Set bsLSaucer=New cvpmBallStack
	    bsLSaucer.InitSaucer LSaucer,8,141,14
		bsLSaucer.InitExitSnd Soundfx("fx_kicker",DOFContactors), Soundfx("fx2_solenoid",DOFContactors)
		
 	

	Set bsRSaucer=New cvpmBallStack
        bsRSaucer.InitSaucer RSaucer,7,172,15

	    bsRSaucer.InitExitSnd Soundfx("fx_kicker",DOFContactors), Soundfx("fx2_solenoid",DOFContactors)   
		


	vpmNudge.TiltSwitch=15
	vpmNudge.Sensitivity=5
	'vpmNudge.TiltObj=Array(BumperL, BumperM, BumperR, LeftSlingshot, RightSlingshot)
End Sub

Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftTiltKey Then Nudge 90, 2
	If keycode = RightTiltKey Then Nudge 270, 2
	If keycode = CenterTiltKey Then	Nudge 0, 2

	If keycode = LeftFlipperKey Then LFPress = 1
	If keycode = RightFlipperKey Then rfpress = 1

	If vpmKeyDown(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.PullBack: PlaySoundAt "fx_plungerpull", Plunger: 	End If

    if keycode = LeftMagnasave then
		controller.switch(1) = True
	end if


If keycode = RightMagnaSave and EnableRightMagnasave = 1 then
		'textbox001.visible = 1
		lutpos = lutpos + 1 : If lutpos > ubound(luts) Then lutpos = 0 : end if
		table1.ColorGradeImage = luts(lutpos)
		'dim tekst : tekst = "lutpos:" & lutpos & " " & luts(lutpos)
		'textbox001.text = tekst
		'vpmTimer.AddTimer 2000, "If textbox001.text =" + chr(34) + tekst + chr(34) + " then textbox001.visible = 0'"
	End if
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then Plunger.Fire: PlaySoundAt "fx_plunger", Plunger

	If keycode = LeftFlipperKey Then 
		lfpress = 0
		leftflipper.eostorqueangle = EOSA
		leftflipper.eostorque = EOST
	End If
	If keycode = RightFlipperKey Then 
		rfpress = 0
		rightflipper.eostorqueangle = EOSA
		rightflipper.eostorque = EOST
	End If
        if keycode = LeftMagnasave then
		controller.switch(1) = False
	end if

	If vpmKeyUp(keycode) Then Exit Sub

End Sub

Sub Drain_Hit()
	PlaySoundAt "fx_drain", Drain 
	RF.PolarityCorrect Activeball
	LF.PolarityCorrect Activeball
	bstrough.addball me
EngageRoulette

End Sub



'
'******************************************************
'						Saucer
'******************************************************


Sub LSaucer_Hit

bsLSaucer.addball 0
playsound"fx_kicker"
  If l43.state=1 then EngageRoulette

end sub



Sub RSaucer_Hit
bsRSaucer.addball 0
playsound"fx_kicker" 
  If l43a.state=1 then EngageRoulette
End Sub 





''***********************************
'' Bumpers
''***********************************



Sub BumperL_Hit
	vpmTimer.PulseSw 24
	playsoundAtVol SoundFX("fx2_bumper1",DOFContactors), BumperL, VolBump
	
End Sub

Sub BumperM_Hit
	vpmTimer.PulseSw 23
	playsoundAtVol SoundFX("fx2_bumper2",DOFContactors), BumperM, VolBump
	
End Sub

Sub BumperR_Hit
	vpmTimer.PulseSw 22
	playsoundAtVol SoundFX("fx2_bumper3",DOFContactors), BumperR, VolBump
	
End Sub



'
'Wire Triggers

Sub SW14_Hit:Controller.Switch(14)=1:End Sub
Sub SW14_unHit:Controller.Switch(14)=0:End Sub
Sub SW13_Hit:Controller.Switch(13)=1:End Sub
Sub SW13_unHit:Controller.Switch(13)=0:End Sub
Sub SW18_Hit:Controller.Switch(18)=1:End Sub
Sub SW18_unHit:Controller.Switch(18)=0:End Sub
Sub SW19_Hit:Controller.Switch(19)=1:End Sub
Sub SW19_unHit:Controller.Switch(19)=0:End Sub
Sub SW28_Hit:Controller.Switch(28)=1:End Sub
Sub SW28_unHit:Controller.Switch(28)=0:End Sub
Sub SW29_Hit:Controller.Switch(29)=1:End Sub
Sub SW29_unHit:Controller.Switch(29)=0::End Sub
Sub SW30_Hit:Controller.Switch(30)=1:End Sub
Sub SW30_unHit:Controller.Switch(30)=0::End Sub
Sub SW31_Hit:Controller.Switch(31)=1:End Sub
Sub SW31_unHit:Controller.Switch(31)=0::End Sub
Sub SW32_Hit:Controller.Switch(32)=1:End Sub
Sub SW32_unHit:Controller.Switch(32)=0::End Sub



' REBOUNDS

Sub SW41_Hit:vpmTimer.PulseSw(27):End Sub     'rebounds
Sub SW42_Hit:vpmTimer.PulseSw(27):End Sub     'rebounds
Sub SW43_Hit:vpmTimer.PulseSw(27):End Sub     'rebounds

Sub sw17_Hit:vpmTimer.PulseSw(17):End Sub



'Spinners
Sub sw12_Spin : vpmTimer.PulseSw (12) :PlaySoundAtVol "fx2_spinner", sw12, VolSpin: End Sub

'***********Rotate Spinner

'*** PI returns the value for PI
Function PI()
	PI = 4*Atn(1)
End Function

Dim Angle

Sub SpinnerTimer_Timer
    SpinnerRod.TransZ = -sin( (sw12.CurrentAngle+180) * (2*3.14/360)) * 5
    SpinnerRod.TransX = (sin( (sw12.CurrentAngle- 90) * (2*3.14/360)) * -5)
	Pgate001.rotx = -Gate001.currentangle*0.5
	Pgate002.rotx = -Gate002.currentangle*0.5

	Dim SpinnerRadius: SpinnerRadius=7

	SpinnerRod.TransZ = (cos((sw12.CurrentAngle + 180) * (PI/180))+1) * SpinnerRadius
	SpinnerRod.TransY = sin((sw12.CurrentAngle) * (PI/180)) * -SpinnerRadius
End Sub
'
'
Dim Step5, Step22, Step27, Step28, Step29, Step30
'
Sub sw5a_Hit
	vpmTimer.PulseSw (5)
	sw5target.TransX=-5		
	sw5a.timerenabled = 1
	Step5 = 0
end sub

Sub sw5a_timer
	Select Case Step5
		Case 3:	sw5target.TransX=3
		Case 4: sw5target.TransX=-4
		Case 5: sw5target.TransX=2
		Case 6: sw5target.TransX=-3
		Case 7: sw5target.TransX=1
		Case 8: sw5target.TransX=-2
		Case 9: sw5target.TransX=0
		Case 10: sw5target.TransX=-1: sw5a.timerenabled = 0
	End Select
	Step5=Step5 + 1
End Sub
'
Sub sw22_Hit
	vpmTimer.PulseSw (22)
	sw22target.TransX=-5		
	sw22.timerenabled = 1
	Step22 = 0
end sub

Sub sw22_timer
	Select Case Step22
		Case 3:	sw22target.TransX=3
		Case 4: sw22target.TransX=-4
		Case 5: sw22target.TransX=2
		Case 6: sw22target.TransX=-3
		Case 7: sw22target.TransX=1
		Case 8: sw22target.TransX=-2
		Case 9: sw22target.TransX=0
		Case 10: sw22target.TransX=-1: sw22.timerenabled = 0
	End Select
	Step22=Step22 + 1
End Sub
'
Sub sw27_Hit
	vpmTimer.PulseSw (27)
	sw27target.TransX=5		
	sw27.timerenabled = 1
	Step27 = 0
end sub

Sub sw27_timer
	Select Case Step27
		Case 3:	sw27target.TransX=-3
		Case 4: sw27target.TransX=4
		Case 5: sw27target.TransX=-2
		Case 6: sw27target.TransX=3
		Case 7: sw27target.TransX=-1
		Case 8: sw27target.TransX=2
		Case 9: sw27target.TransX=0
		Case 10: sw27target.TransX=0: sw27.timerenabled = 0
	End Select
	Step27=Step27 + 1
End Sub
'

'

'

'
Sub SolKnocker(Enabled)
	If Enabled Then PlaySound SoundFX("Fx2_Knocker",DOFKnocker)
End Sub
'
''**********Sling Shot Animations
'' Rstep and Lstep  are the variables that increment the animation
''****************
Dim RStep, Lstep
'
Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx2_slingshot1",DOFContactors), sling1
  	vpmtimer.PulseSw(20)
    rsling2.Visible = 1
    rsling1.Visible = 0
    sling1.Rotx = 26

    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
       Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.Rotx = 14
       Case 4:RSLing2.Visible = 0:sling1.Rotx = 2
       Case 5:sLing1.Rotx = -20 :RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub
'
Sub LeftSlingshot_Slingshot
    PlaySoundAt SoundFX("fx2_slingshot2",DOFContactors),sling2
	vpmtimer.pulsesw(21)
    lsling2.Visible = 1
    lsling1.Visible = 0
    sling2.Rotx = 26
    LStep = 0
    LeftSlingshot.TimerEnabled = 1
End Sub

Sub LeftSlingshot_Timer
    Select Case LStep
        Case 3:lsling1.Visible = 0:lsling2.Visible = 1:sling2.Rotx = 14     
        Case 4:lsling2.Visible = 0:sling2.Rotx = 2       '          
        Case 5:sling2.Rotx= -20:LeftSlingshot.TimerEnabled = 0    
    End Select
    LStep = LStep + 1
End Sub
'
''-------------------------------------
'' Map lights into array
'' Set unmapped lamps to Nothing
''-------------------------------------
Set Lights(1)  = l1                   ' card 5
Set Lights(2)  = l2                   ' card 9
Set Lights(3)  = l3                   ' card 10
Set Lights(4)  = l4                   ' card ace
'Set Lights(5)  = l5                   ' light scorewheel
Set Lights(6)  = l6                   ' 5000 right
Set Lights(7)  = l7                   ' arrow left 5000
Set Lights(8)  = l8                   ' arrow left special
Set Lights(9)  = l9                   ' arrow right
Set Lights(10) = l10                  '  chip value 25 
Set Lights(11) = l11                  '  chip value 125
Set Lights(12) = l12                  ' joker 1
                                                                         '13 not used
Set Lights(14) = l14                  ' 500
Set Lights(15) = l15                  ' 2x multiplier
                                                                         '16 not used
Set Lights(17) = l17                  ' card 6
Set Lights(18) = l18                  ' 4 bonus advance left
Set Lights(19) = l19                  ' card boer
Set Lights(20) = l20                  ' 60 thousands
'Set Lights(21) = l21                  ' light scorewheel
Set Lights(22) = l22                  ' 10.000
Set Lights(23) = l23                  ' arrow left 10.000
Set Lights(24) = l24                  ' sacrifice light apron
Set Lights(25) = l25                  ' chip in sequence
Set Lights(26) = l26                  ' chip value 50
                                                                      ' 27  backglass match
Set	Lights(28) = l28                  ' joker2
                                                                      ' 29  backglass high score
Set Lights(30) = l30                  ' 1000
Set Lights(31) = l31                  ' 3x multiplier
                                                                      ' 32 not used
Set Lights(33) = l33                  ' card 7
Set Lights(34) = l34                  ' 5000  left
Set Lights(35) = l35                  ' card queen                                                                    
                                                                      ' 36 not Used 
'Set Lights(37) = l37                  ' light scorewheel
Set Lights(38) = l38                  ' 15.000
Set Lights(39) = l39                  ' arrow left 15.000
Set Lights(40) = l40                  ' chip special
Set Lights(41) = l41                  ' when lit   cards in sequence
Set Lights(42) = l42                  ' chip value 75
	Lights(43) = Array(L43,L43a)      ' spin wheel when lit
Set Lights(44) = l44                  ' joker 3
                                                                       ' 45 backglass game over
Set Lights(46) = l46                  ' 2000
Set Lights(47) = l47                  ' 5x multiplier
                                                                       ' 48 not used
Set Lights(49) = l49                  ' card 9
Set Lights(50) = l50                  ' 25.000
Set Lights(51) = l51                  ' card king
Set	Lights(52) = l52                  ' add-a-ball
'Set Lights(53) = l53                  ' light scorewheel
Set	Lights(54) = l54                  ' 20.000
Set	Lights(55) = l55                  ' arrow left 20.000
Set Lights(56) = l56                  ' special cards in sequence
Set Lights(57) = l57                  ' special right
Set Lights(58) = l58                  ' chip value 100

Set Lights(59) = l59                  '  coin

Set Lights(60) = l60                  '  joker 4
                                                                        ' 61 backglass Tilt
Set Lights(62) = l62                  '  3000
Set Lights(63) = l63                  '  10x multiplier
                                                                        ' 64 not used
Set Lights(65) = l65                  '  3 thousand
Set Lights(66) = l66                  '  5 thousand
Set Lights(67) = l67                  '  9 thousand
Set Lights(68) = l68                  ' 13 thousand
                                                                        ' 69   backglass -2
                                                                        ' 70   backglass 2
                                                                        ' 71   backglass 6
                                                                        ' 72   backglass light
Set Lights(81) = l81                  '  2 thousand
Set Lights(82) = l82                  '  6 thousand
Set Lights(83) = l83                  ' 10 thousand
Set Lights(84) = l84                  ' 14 thousand
                                                                        ' 85   backglass -1
                                                                        ' 86   backglass 3
                                                                        ' 87   backglass 87
                                                                        ' 88   backglass light
                                                                        ' 89 ... 96 not used
Set Lights(97) = l97                  '  1 thousand
Set Lights(98) = l98                  '  7 thousand
Set Lights(99) = l99                  ' 11 thousand
Set Lights(100) = l100                ' 15 thousand
                                                                         ' 101 backglass 0
                                                                         ' 102 backglass 4
                                                                         ' 103 backglass 8
    Set Lights(104) = L104   ' set as starlight                          ' 104 backglass light
                                                                         ' 105...112 not used
Set Lights(113) = l113                '  4 thousand
Set Lights(114) = l114                '  8 thousand
Set Lights(115) = l115                ' 12 thousand
Set Lights(116) = l116  

                                                                         ' 117 backglass 1
                                                                         ' 118 backglass 5
                                                                         ' 119 backglass 9






'*  Wheel Illumination
Lights(5) = Array(l5,l5b,l5c,l5d)
Lights(21) = Array(l21,l21b,l21c,l21d)
Lights(37) = Array(l37,l37b,l37c)
Lights(53) = Array(l53,l53b,l53c)



Dim BootCount:BootCount = 0

Sub BootTable_Timer()
	If BootCount = 0 Then
		BootCount = 1
       ' PlaySound "fx2_flapopen"


		
	Else

PlaySound "target"
		'*****GI Lights On
		dim xx
		For each xx in GI:xx.State = 1: Next
		'For each xx in layer1col: xx.image = "layer1": Next
		'For each xx in layer2col: xx.image = "layer2": Next
		'For each xx in layer3col: xx.image = "layer3": Next
		'For each xx in bumpskirts: xx.image = "skirts": Next
		'plasticright.image="plasticright"
		'plasticleft.image="plasticleft"
		'bumpcaps.image="bumpcaps"
		bracketla.image="bracketla"
		bracketlb.image="bracketlb"
		bracketra.image="bracketra"
		bracketrb.image="bracketrb"
		'lsling.image="layer2"
		'rsling.image="layer2"
playfieldoff.visible=0
plastics.image="plastics1on"
rubbers.image="rubberswhiteon297"
primitive141.image="Bally Kicker double"
primitive004.image="Bally Kicker double"
woodcab.image="camwood77"
pegsblue.image="pegsblueon"
metals.image="metalon"
Primitive013.image="bumperspon1"
Primitive012.image="bumperspon1"
Primitive42.image="bumperspon1"
brackets.image="bracketson"
Primitive011.image="apron127on"
sw17.image="Targetyellow1"
bumperlight1.state=1
bumperlight2.state=1
bumperlight3.state=1
light001.state=1
light002.state=1
light003.state=1




primitive006.image="instruction 4 pl"
primitive007.image="4pl 5ball on"



		me.enabled = false
	End If
End Sub
'
'*****************************************
'			FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh001.RotZ = LeftFlipper.currentangle
	FlipperRSh001.RotZ = RightFlipper.currentangle
	'If l59.state=1 Then
		'if light001.state=1 Then
		'apron.image="apron_lit"
		'apronlamp.image="apronlamp_lit"
		'end if
	'Else
		'if light001.state=1 Then
		'apron.image="apron"
		'apronlamp.image="apronlamp"
		'end if
	'End If
End Sub

'*****************************************
'			BALL SHADOW
'*****************************************
'Dim BallShadow
'BallShadow = Array (BallShadow001,BallShadow002,BallShadow003,BallShadow004,BallShadow005)

'Sub BallShadowUpdate_timer()
  '  Dim BOT, b
   ' BOT = GetBalls
   ' ' hide shadow of deleted balls
   ' If UBound(BOT)<(tnob-1) Then
     '  For b = (UBound(BOT) + 1) to (tnob-1)
           ' BallShadow(b).visible = 0
       ' Next
   ' End If
   ' ' exit the Sub if no balls on the table
   ' If UBound(BOT) = -1 Then Exit Sub
   ' ' render the shadow for each ball
  '  For b = 0 to UBound(BOT)
       ' If BOT(b).X < Table1.Width/2 Then
          ' BallShadow(b).X = ((BOT(b).X) + ((BOT(b).X - (Table1.Width/2))/7))
       ' Else
           ' BallShadow(b).X = ((BOT(b).X) + ((BOT(b).X - (Table1.Width/2))/7))
      '  End If
      '  ballShadow(b).Y = BOT(b).Y + 10
      '  If BOT(b).Z > 20 Then
       '     BallShadow(b).visible = 1
      ' Else
         '  BallShadow(b).visible = 0
      '  End If
   ' Next
'End Sub


'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/40) + ((BOT(b).X - (Table1.Width/2))/40)) + 1
		Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/40) + ((BOT(b).X - (Table1.Width/2))/40)) - 1
        End If
        ballShadow(b).Y = BOT(b).Y + 30
		BallShadow(b).Z = BOT(b).Z + 1
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

''******************************
'' Diverse Collection Hit Sounds
''******************************
'
'Sub aBumpers_Hit (idx): PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, pan(ActiveBall): End Sub
Sub aRollovers_Hit(idx):PlaySound "fx2_sensor", 0, Vol(ActiveBall)*VolRol, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall)*VolGates, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aMetals_Hit(idx):PlaySound "fx2_MetalHit2", 0, Vol(ActiveBall)*VolMetal, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub Metals2_Hit(idx):PlaySound "fx2_Hit_Metal", 0, Vol(ActiveBall)*VolMetal, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx2_rubber", 0, Vol(ActiveBall)*VolRB, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubbers_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall)*VolRH, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall)*VolPo, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall)*VolPi, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall)*VolPlast, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aTargets_Hit(idx):PlaySound "drophit2", 0, Vol(ActiveBall)*VolTarg, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub
Sub aWoods_Hit(idx):PlaySound "fx2_Woodhit3", 0, Vol(ActiveBall)*VolWood, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall):End Sub


'
'*************************
 'DIP's added by Inkochnito
 '*************************
 Sub editDips
    Dim vpmDips : Set vpmDips=New cvpmDips
    With vpmDips
       .AddForm 700, 400, "Speak Easy - DIP switches"
       .AddFrame 0, 0, 100, "Maximum credits", &H03000000, Array("10 credits", 0, "15 credits", &H01000000, "25 credits", &H02000000, "40 credits", &H03000000)                                                             'dip 25&26
       .AddFrame 0, 76, 100, "Maximum balls", &H00006000, Array("6 balls", 0, "7 balls", &H00002000, "8 balls", &H00004000, "9 balls", &H00006000)                                                                          'dip 14&15
       .AddFrame 0, 152, 100, "Balls per game", &HC0000000, Array("2 balls", &HC0000000, "3 balls", 0, "4 balls", &H80000000, "5 balls", &H40000000)                                                                        'dip 31&32
       .AddFrame 115, 0, 250, "10 thru ace card special adjust", &H00600000, Array("special lites with 125 chip value", 0, "special lites with 100 chip value", &H00400000, "special lites with 75 chip value", &H00600000) 'dip 22&23
       .AddFrame 115, 61, 250, "Top card 5 thru 9 sequence lite", &H00100000, Array("making 5 thru 9 only keeps lite on", 0, "making 5 thru 9 or 9 thru 5 keeps lite on", &H00100000)                                       'dip 21
       .AddFrame 115, 107, 250, "Outlane special adjust", 32768, Array("special lite goes out after being made", 0, "special lite stays on after being made", 32768)                                                        'dip 16
       .AddFrame 115, 153, 250, "Replay limit", &H10000000, Array("1 replay per game", 0, "unlimited replays", &H10000000)                                                                                                  'dip 29
       .AddChk 115, 200, 190, Array("Match feature", &H08000000)                                                                                                                                                            'dip 28
       .AddChk 115, 215, 190, Array("Credits displayed", &H04000000)                                                                                                                                                        'dip 27
       .AddLabel 30, 240, 350, 15, "Set selftest position 16,17,18 and 19 to 03 for the best gameplay."
       .AddLabel 50, 255, 300, 15, "After hitting OK, press F3 to reset game with new settings."
       .ViewDips
    End With
 End Sub
 
 Set vpmShowDips=GetRef("editDips")

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_Exit:Controller.Stop:End Sub



' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioFade(ball) ' Can this be together with the above function ?
  Dim tmp
  tmp = ball.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

ReDim ArchRolling(tnob)
InitArchRolling

Dim ArchHit

Sub toparch_Hit
	Archhit = 1
End Sub

Sub NotOnArch_Hit
	ArchHit = 0
End Sub

Sub NotOnArch_unHit
	ArchHit = 0
End Sub

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub InitArchRolling
	Dim i
	For i = 0 to tnob
		ArchRolling(i) = False
	Next
End Sub

Sub RollingTimer_Timer()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("fx_Rolling_Wood" & b)
		StopSound("fx_Rolling_Plastic" & b)
		StopSound("fx_Rolling_Metal" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
	For b = 0 to UBound(BOT)
		If BallVel(BOT(b) ) > 1 Then

			' ***Ball on WOOD playfield***
			if BOT(b).z < 27 Then 
				PlaySound("fx_Rolling_Wood" & b), -1, Vol(BOT(b) )/2, AudioPan(BOT(b) )/5, 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
				rolling(b) = True
				StopSound("fx_Rolling_Plastic" & b)
				StopSound("fx_Rolling_Metal" & b)
			' ***Ball on PLASTIC ramp***
			Elseif  BOT(b).z > 28 Then 
				PlaySound("fx_Rolling_Plastic" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) 
				rolling(b) = True
				StopSound("fx_Rolling_Wood" & b)
				StopSound("fx_Rolling_Metal" & b)
			Else
				if rolling(b) = true Then
					rolling(b) = False
					StopSound("fx_Rolling_Wood" & b)
					StopSound("fx_Rolling_Plastic" & b)
					StopSound("fx_Rolling_Metal" & b)
				end if
			End If
		Else
			If rolling(b) = True Then
				StopSound("fx_Rolling_Wood" & b)
				StopSound("fx_Rolling_Plastic" & b)
				StopSound("fx_Rolling_Metal" & b)
				rolling(b) = False
			End If

		End If

		'***Ball Drop Sounds***

		If BOT(b).VelZ < -1 and BOT(b).z < 50 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySoundAtBOTBallZ "fx_ball_drop" & b, BOT(b)
		End If

		If BallVel(BOT(b) ) > 1 AND ArchHit =1 Then
			ArchRolling(b) = True
			PlaySound("ArchHitA" & b),   0, (BallVel(BOT(b))/15)^5 * 1, AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
			PlaySound("ArchRollA" & b), -1, (BallVel(BOT(b))/30)^5 * 1, AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		Else
			If ArchRolling(b) = True Then
			StopSound("ArchRollA" & b)
			ArchRolling(b) = False
			End If
		End If
	Next
End Sub

Sub PlaySoundAtBOTBall(sound, BOT)
		PlaySound sound, 0, Vol(BOT), AudioPan(BOT), 0, Pitch(BOT), 0, 1, AudioFade(BOT)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
		PlaySound sound, 0, ABS(BOT.velz)/17, AudioPan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF, RF1)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
			case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub
	
	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next		
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
				if DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				if DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr 
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				if DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				if DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						if DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			if DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				if IsEmpty(PolarityOut(0) ) then 
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else 
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					if BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if	
					if Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					if PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline				
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'if DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class

'================================
'Helper Functions


Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class


Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim RF1 : Set RF1 = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF, RF1)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 80
	Next

	'"Polarity" Profile
'"Polarity" Profile<br>
'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.1, 0
'	AddPt "Polarity", 2, 0.14, -2.25
'	AddPt "Polarity", 3, 0.2, -2.25
'	AddPt "Polarity", 4, 0.28, -3.25
'	AddPt "Polarity", 5, 0.31, -3.25
'	AddPt "Polarity", 6, 0.34, -3.75
'	AddPt "Polarity", 7, 0.37, -3.75
'	AddPt "Polarity", 8, 0.4, -4.5
'	AddPt "Polarity", 9, 0.45, -3.5
'	AddPt "Polarity", 10, 0.48, -3.5
'	AddPt "Polarity", 11, 0.51, -3.75
'	AddPt "Polarity", 12, 0.55, -3.75
'	AddPt "Polarity", 13, 0.58, -3
'	AddPt "Polarity", 14, 0.6, -2.75
'	AddPt "Polarity", 15, 0.62, -2.75
'	AddPt "Polarity", 16, 0.65, -2.5
'	AddPt "Polarity", 17, 0.8, -2
'	AddPt "Polarity", 18, 0.85, -1.9
'	AddPt "Polarity", 19, 1.0, -1
'	AddPt "Polarity", 20, 1.2, 0

	'rf.report "Polarity"
	AddPt "Polarity", 0, 0, -2.7
	AddPt "Polarity", 1, 0.16, -2.7	
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7	'4.2
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7 '4.2
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8'-2.1896
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
Sub TriggerRF1_Hit() : RF1.Addball activeball : End Sub
Sub TriggerRF1_UnHit() : RF1.PolarityCorrect activeball : End Sub

Sub RDampen_Timer()
	Cor.Update
End Sub

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub


dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub
	

End Class

'Tracks ball velocity for judging bounce calculations & angle
'apologies to JimmyFingers is this is what his script does. I know his tracks ball velocity too but idk how it works in particular
dim cor : set cor = New CoRTracker
cor.debugOn = False
'cor.update() - put this on a low interval timer
Class CoRTracker
	public DebugOn 'tbpIn.text
	public ballvel

	Private Sub Class_Initialize : redim ballvel(0) : End Sub 
	'TODO this would be better if it didn't do the sorting every ms, but instead every time it's pulled for COR stuff
	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs
		if uBound(allballs) < 0 then if DebugOn then str = "no balls" : TBPin.text = str : exit Sub else exit sub end if: end if
		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			if DebugOn then 
				dim s, bs 'debug spacer, ballspeed
				bs = round(BallSpeed(b),1)
				if bs < 10 then s = " " else s = "" end if
				str = str & b.id & ": " & s & bs & vbnewline 
				'str = str & b.id & ": " & s & bs & "z:" & b.z & vbnewline 
			end if
		Next
		if DebugOn then str = "ubound ballvels: " & ubound(ballvel) & vbnewline & str : if TBPin.text <> str then TBPin.text = str : end if
	End Sub
End Class






'------------------------------
'-----  Card Target Bank  -----
'------------------------------
Sub SolCardTargetsReset(Enabled)
	if enabled Then
		PlaySound SoundFX("fx_resetdrop",DOFContactors),0,1,0,0.05
		Controller.switch(37) = False
		Controller.switch(38) = false
		Controller.switch(39) = false
		Controller.switch(40) = false
		Controller.switch(36) = false
	end if
End SUb

Sub CardGateTimer_Timer
	if Controller.switch(37) Then
		P001.rotx= -90
        
else
		P001.rotx= -CardGate1.currentangle

	end if
	if Controller.switch(38) Then
		P002.rotx = -90

	else
		P002.rotx = -CardGate2.currentangle
	end if
	if Controller.switch(39) Then
		P003.rotx = -90

	else
		P003.rotx = -CardGate3.currentangle
	end if
	if Controller.switch(40) Then
		P004.rotx = -90

	else
		P004.rotx = -CardGate4.currentangle
	end if
	if Controller.switch(36) Then
		P005.rotx = -90
	else
		P005.rotx = -CardGate5.currentangle
	end if
	
End Sub

Const CardHitThreshold = -6	'adjust to change the force needed to lock a card target
Sub CardGate1Trigger_hit
	if activeball.vely < CardHitThreshold Then
		if not Controller.switch(37) then 
			PlaySound SoundFX("fx_diverter",DOFContactors), 0,0.25   ', AudioPan(P005),0.05
			Controller.switch(37) = True

		end if

     else
          if not Controller.switch(37) then 
               PlaySound SoundFX("fx_Flipperdown1",DOFContactors), 0,0.25   ', AudioPan(P001),0.05

	end if

	end if
End Sub
Sub CardGate2Trigger_hit
	if activeball.vely < CardHitThreshold Then
		if not Controller.switch(38) then 
			PlaySound SoundFX("fx_diverter",DOFContactors), 0,0.25   ', AudioPan(P005),0.05
			Controller.switch(38) = True

		end if
else
          if not Controller.switch(38) then 
               PlaySound SoundFX("fx_Flipperdown1",DOFContactors), 0,0.25   ', AudioPan(P001),0.05

	end if
	end if
End Sub
Sub CardGate3Trigger_hit
	if activeball.vely < CardHitThreshold Then
		if not Controller.switch(39) then 
			PlaySound SoundFX("fx_diverter",DOFContactors), 0,0.25   ', AudioPan(P005),0.05
			Controller.switch(39) = True

		end if
else
          if not Controller.switch(39) then 
               PlaySound SoundFX("fx_Flipperdown1",DOFContactors), 0,0.25   ', AudioPan(P001),0.05

	end if
	end if
End Sub
Sub CardGate4Trigger_hit
	if activeball.vely < CardHitThreshold Then
		if not Controller.switch(40) then 
			PlaySound SoundFX("fx_diverter",DOFContactors), 0,0.25   ', AudioPan(P005),0.05
			Controller.switch(40) = True

		end if
else
          if not Controller.switch(40) then 
               PlaySound SoundFX("fx_Flipperdown1",DOFContactors), 0,0.25   ', AudioPan(P001),0.05

	end if
	end if
End Sub
Sub CardGate5Trigger_hit
	if activeball.vely < CardHitThreshold Then
		if not Controller.switch(36) then 
			PlaySound SoundFX("fx_diverter",DOFContactors), 0,0.25   ', AudioPan(P005),0.05
			Controller.switch(36) = True

		end if
else
          if not Controller.switch(36) then 
               PlaySound SoundFX("fx_Flipperdown1",DOFContactors), 0,0.25   ', AudioPan(P001),0.05

	end if
	end if
End Sub

 

 Sub trigger26_Hit() : controller.switch(26)=1 : Me.TimerEnabled=1 : Playsound "fx2_sensor" : End sub
 Sub trigger26_unHit() : controller.switch(26)=0 : End sub


'******************************************************
'				Gopher Wheel
'******************************************************

Dim roulette_step, roulette_time, wheelangle, ball, isOn, isOff, holeangle, holeanglemod, prevholeangle, whxx
roulette_step = 0
roulette_time = 0
wheelangle = 0 
isOn = True
isOff = True
holeangle = 0 

Dim RouletteBall, CoconutBall

Dim GWHoles
GWHoles = Array (GW0,GW1,GW2,GW3,GW4,GW5,GW6,GW7,GW8,GW9,GW10,GW11)


Sub Init_Roulette()
	GWRamp.collidable=0
	Set RouletteBall = GW0.CreateSizedBallWithMass(20,16)'(23,1.0*((23*2)^3)/125000)
	RouletteBall.image = "Ball_HDR"
	RouletteSw.enabled = 1
End Sub

Sub EngageRoulette:isOn=True:isOff=true :roulette.enabled = 1: End Sub         ':roulette.enabled = 1:

Sub Roulette_Timer()
	GopherWheel.Rotz =  180 - wheelangle - 9
	GopherCone1.RotY =  30 + wheelangle 
	'GopherKicker.Roty = 180 + wheelangle
	'CapGW.RotY =   96 + wheelangle
    sproulette.Rotz = -180 +wheelangle +8
	wheelangle = wheelangle + roulette_step

	holeangle = wheelangle mod 30
	prevholeangle = holeangle
	
	if roulette_time < 900 Then '1500
		if isOn = True And isOff = True Then
			For each ii in Rswitches:Controller.switch(ii)=0:next
			roulettesw.enabled = false
			For ii = 0 to 11
				GWHoles(ii).kick 30*(ii+1)+60, (rnd(4))+12
				GWHoles(ii).enabled = false
			Next			
			GWRamp.collidable = 1
			ttGopherWheel.MotorOn = True
			isOn = False
			Playsound "arch"
            DOF 101, DOFPulse 
		End If
		If wheelangle > 360 Then: wheelangle=wheelangle-360:End If
		roulette_step = roulette_step + 0.05
		If roulette_step > 0.75 Then: roulette_step = 0.75:End If
	Else
		if isOff = True Then
			ttGopherWheel.MotorOn = False
			isOff = False
			StopSound "arch"
		End If
		roulette_step = roulette_step - 0.0005
		If roulette_step < 0.15 and holeangle = 0 Then
			GWRamp.collidable = 0
			roulette_step=0
			roulette.enabled = false
			roulettesw.enabled = true
			roulette_time = 0
		End If			
	End If

	roulette_time = roulette_time + 1
End Sub


Dim RouletteAtRest, RouletteBallAngle, RouletteX, RouletteY, RouletteAtn2, Rswitches, Rletters(12)
Const RouletteCenterX = 444
Const RouletteCenterY = 1249
RouletteAtRest = 0

Rswitches = array (5,4,5,4,5,2,5,4,5,4,5,3)

Sub GetRouletteSw()
	RouletteBallAngle = CalcAngle(RouletteBall.X,RouletteBall.Y,RouletteCenterX,RouletteCenterY)
	Rletters(0) = wheelangle  + 38 '+ 38
	for ii = 0 to 11
		Rletters(ii) = (Rletters(0) + (ii*30)) mod 360
		If AngleIsNear(RouletteBallAngle,Rletters(ii),15) Then
			Controller.Switch(Rswitches(ii)) = 1
			debug.print Rswitches(ii)
			Exit For
		End If
	next

End Sub


Sub RouletteSw_Timer()
	'debug.print BallVel(RouletteBall)
	If BallVel(RouletteBall) < 2 Then
		For each ii in GWKickers
			ii.enabled = true
		next
	End If
End Sub


Sub GW0_hit():GetRouletteSw :debug.print "kicker 0":End Sub
Sub GW1_hit():GetRouletteSw:debug.print "kicker 1":End Sub
Sub GW2_hit():GetRouletteSw:debug.print "kicker 2":End Sub
Sub GW3_hit():GetRouletteSw:debug.print "kicker 3":End Sub
Sub GW4_hit():GetRouletteSw:debug.print "kicker 4":End Sub
Sub GW5_hit():GetRouletteSw:debug.print "kicker 5":End Sub
Sub GW6_hit():GetRouletteSw:debug.print "kicker 6":End Sub
Sub GW7_hit():GetRouletteSw:debug.print "kicker 7":End Sub
Sub GW8_hit():GetRouletteSw:debug.print "kicker 8":End Sub
Sub GW9_hit():GetRouletteSw:debug.print "kicker 9":End Sub
Sub GW10_hit():GetRouletteSw:debug.print "kicker 10":End Sub
Sub GW11_hit():GetRouletteSw:debug.print "kicker 11":End Sub

 '*****************************************************************
 'Functions
 '*****************************************************************

'*** AngleIsNear returns true if the testangle is within range degrees of the target angle for 0 to 360 degree scale

Function AngleIsNear(testangle, target, range)
	If target <= range Then
		AngleIsNear = ( (testangle + (range*2) ) mod 360 >= target + range ) AND ( (testangle + (range*2) ) mod 360 <= target + (range*3) )
	ElseIf target >= 360 - range Then
		AngleIsNear = ( (testangle - (range*2) ) mod 360 >= target - (range*3) ) AND ( (testangle - (range*2) ) mod 360 <= target - range )
	Else
		AngleIsNear = (testangle >= target - range) AND (testangle <= target + range)
	End If
End Function

'*** CalcAngle returns the angle of a point (x,y) on a circle with center (centerx, centery) with angle 0 at 3 o'clock

Function CalcAngle(x,y,centerX,centerY)
	
	Dim tmpAngle
	
	tmpAngle = Atan2(x - centerX, y - centerY) * 180 / (4*Atn(1))

	If tmpAngle < 0 Then	
		CalcAngle = tmpAngle + 360
	Else	
		CalcAngle = tmpAngle
	End If

End Function

'*** Atan2 returns the Atan2 for a point on a circle (x,y)

Function Atan2(x,y)

	If x > 0 Then
		Atan2 = Atn(y/x)
	ElseIf x < 0 Then
		Atan2 = Sgn(y) * ((4*Atn(1)) - Atn(Abs(y/x)))
	ElseIf y = 0 Then
		Atan2 = 0
	Else
		Atan2 = Sgn(y) * (4*Atn(1)) / 2
	End If

End Function

'*** PI returns the value for PI

Function PI()

	PI = 4*Atn(1)

End Function

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function





