'///////////////////////////////////////////////////////////
'			GEMINI by Gottlieb (1978)
'
'
'
' vp10 assembled and scripted by BorgDog, 2015, update 2018
' graphics greatly improved by hauntfreaks and sliderpoint(thanks!)
' load options menu by holding left flipper
' 
'
' DOF support added by arngrim
'
'
'///////////////////////////////////////////////////////////
Option Explicit
Randomize

Const cGameName = "gemini_1977"
Const Ballsize = 50			'used by ball shadow routine

Dim operatormenu, options, optionsX
Dim bumperlitscore
Dim bumperoffscore
Dim balls
Dim replays
Dim Replay1Table(3), Replay2Table(3), Replay3Table(3)
Dim Add10, Add100, Add1000
dim replay1, replay2, replay3
Dim hisc
Dim players, player
Dim credit, freeplay, shadows, clean
Dim score(2)
Dim sreels(2)
Dim state
Dim tilt
Dim tiltsens
Dim target(8)
Dim StarState
Dim Bonus, chimescount
Dim Bonuslight(19)
Dim ballinplay
Dim matchnumb
dim ballrenabled
Dim rep(2)
Dim rst
Dim eg
Dim scn
Dim scn1
Dim bell
Dim i,j,tnumber, light, objekt
Dim awardcount, dbonus

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

'**********************************************************
'********   	OPTIONS		*******************************
'**********************************************************


sub Gemini_init
	LoadEM
	Replay1Table(1)=100000
	Replay1Table(2)=130000
	Replay1Table(3)=180000

	Replay2Table(1)=120000
	Replay2Table(2)=140000
	Replay2Table(3)=190000

	Replay3Table(1)=130000
	Replay3Table(2)=160000
	Replay3Table(3)=190000
	set sreels(1) = ScoreReel1
	set sreels(2) = ScoreReel2
	set target(1)=DTRedL
	set target(2)=DTRedR
	set target(3)=DTwhiteL
	set target(4)=DTwhiteR
	set target(5)=DTGreenL
	set target(6)=DTGreenR
	set target(7)=DTYellowL
	set target(8)=DTYellowR
	set bonuslight(1)=bonus1
	set bonuslight(2)=bonus2
	set bonuslight(3)=bonus3
	set bonuslight(4)=bonus4
	set bonuslight(5)=bonus5
	set bonuslight(6)=bonus6
	set bonuslight(7)=bonus7
	set bonuslight(8)=bonus8
	set bonuslight(9)=bonus9
	set bonuslight(10)=bonus10
	set bonuslight(11)=bonus11
	set bonuslight(12)=bonus12
	set bonuslight(13)=bonus13
	set bonuslight(14)=bonus14
	set bonuslight(15)=bonus15
	set bonuslight(16)=bonus16
	set bonuslight(17)=bonus17
	set bonuslight(18)=bonus18
	set bonuslight(19)=bonus19
	hideoptions
	player=1

	hisc=50000		'SET DEFAULT VALUES PRIOR TO LOADING FROM SAVED FILE
	credit=0
	freeplay=0
	shadows=1
	balls=3
	matchnumb=0
	replays=2
	HSA1=4
	HSA2=15
	HSA3=7
	clean=0

	if ShowDT then
		OptionReel.objrotx=10
		OptionsReel.objrotx=0
		OptionBox.objrotx=-14
		OptionBox.z=25
		OptionReel.z=30
		OptionsReel.z=30
	  else
		OptionReel.objrotx=28
		OptionsReel.objrotx=15
		OptionBox.objrotx=0
		OptionBox.z=0
		OptionReel.z=5
		OptionsReel.z=5
	end If

	loadhs			'LOADS SAVED OPTIONS AND HIGH SCORE IF EXISTING
	UpdatePostIt	'UPDATE HIGH SCORE STICKY

	creditreel.setvalue(credit)
    scorereel1.setvalue(score(1))
    scorereel2.setvalue(score(2))

	Replay1=Replay1Table(Replays)
	Replay2=Replay2Table(Replays)
	Replay3=Replay3Table(Replays)
	RepCard.image = "ReplyCard"&replays

	if balls=3 then
		bumperlitscore=1000
		bumperoffscore=1000
		InstCard.image="InstCard3balls"
	end if
	if balls=5 then
		bumperlitscore=100
		bumperoffscore=100
		InstCard.image="InstCard5balls"
	end if
	If B2SOn then
		for each objekt in backdropstuff : objekt.visible = 0 : next
	End If
	startGame.enabled=true
	tilt=false

	if clean=0 Then
		Wapron.image="GeminiApron"
		Arch.image="GeminiApron"
		PPlungerCover.image="Plungercover"
		Gemini.ColorGradeImage="ColorGradeLUT256x16_ModDeSat"
	  Else
		Wapron.image="CleanApron"
		Arch.image="CleanApron"
		PPlungerCover.image="Plungercover3"
		Gemini.ColorGradeImage="ColorGradeLUT256x16_ConSat"
	end if

	if shadows=1 then
		BallShadowUpdate.enabled=1
		FlipperLSh.visible=1
		FlipperRSh.visible=1
	  else
		BallShadowUpdate.enabled=0
		FlipperLSh.visible=0
		FlipperRSh.visible=0
	end if



    Drain.CreateBall
End sub

sub startGame_timer
	PlaySoundAt "poweron", Plunger
	setBackglass.enabled=true
	me.enabled=false
end sub

sub setBackglass_timer
	gamov.text="Game Over"
	gamov.timerenabled=1
	tilttxt.timerenabled=1
	if matchnumb=0 then
		matchtxt.text="00"
	  else
		matchtxt.text=matchnumb*10
	end if
	Reel100K1.setvalue(int(score(1)/100000))
	Reel100K2.setvalue(int(score(2)/100000))
	IF Credit > 0 Then DOF 122, DOFOn
	for each light in bumperlights:light.state=1:next
	for each light in GIlights:light.state=1:next
	if b2son then 
		Controller.B2ssetCredits Credit
		Controller.B2ssetMatch 34, Matchnumb
		Controller.B2SSetGameOver 35,1
		Controller.B2SSetScore 4, hisc
		Controller.B2SSetScorePlayer 1, Score(1)
		Controller.B2SSetScorePlayer 2, Score(2)
		if score(1)>99999 then Controller.B2SSetScoreRollover 25, 1
		if score(2)>99999 then Controller.B2SSetScoreRollover 26, 1
	end if
	me.enabled=false
end sub

sub gamov_timer
	if state=false then
		If B2SOn then Controller.B2SSetGameOver 35,0			
		gamov.text=""
		gtimer.enabled=true
	end if
	gamov.timerenabled=0
end sub

sub gtimer_timer
	if state=false then
		gamov.text="Game Over"
		If B2SOn then Controller.B2SSetGameOver 35,1
		gamov.timerinterval= (INT (RND*10)+5)*100
		gamov.timerenabled=1
	end if
	me.enabled=0
end sub

sub tilttxt_timer
	if state=false then
		tilttxt.text=""
		If B2SOn then Controller.B2SSetTilt 33,0	
		ttimer.enabled=true
	end if
	tilttxt.timerenabled=0
end sub

sub ttimer_timer
	if state=false then
		tilttxt.text="TILT"
		If B2SOn then Controller.B2SSetTilt 33,1
		tilttxt.timerenabled=1
		tilttxt.timerinterval= (INT (RND*10)+5)*100
	end if
	me.enabled=0
end sub

Sub Gemini_KeyDown(ByVal keycode)
   
' **** ball control script
	if keycode = 46 then' C Key
		If contball = 1 Then
			contball = 0
		  Else
			contball = 1
		End If
	End If

	if keycode = 48 then 'B Key
		If bcboost = 1 Then
			bcboost = bcboostmulti
		  Else
			bcboost = 1
		End If
	End If

	if keycode = 203 then Cleft = 1' Left Arrow

	if keycode = 200 then Cup = 1' Up Arrow

	if keycode = 208 then Cdown = 1' Down Arrow

	if keycode = 205 then Cright = 1' Right Arrow

'  **** end ball control section

	if keycode=AddCreditKey then
		if freeplay=1 or credit>15 Then
			playsoundat "coin3", Drain
		  Else
			playsoundat "coin6", Drain
			addcredit
		end If

    end if

    if keycode=StartGameKey and (Credit>0 or freeplay=1) And Not HSEnterMode=true and Not Startgame.enabled then
	  if state=false then
		if freeplay=0 then 
			credit=credit-1
			If credit < 1 Then DOF 122, DOFOff
			creditreel.setvalue(credit)
		end if
		playsound "click"
		ballinplay=1
		If B2SOn Then 
			Controller.B2ssetCredits Credit
			Controller.B2sStartAnimation "Startup"
			Controller.B2ssetballinplay 32, Ballinplay
			Controller.B2ssetplayerup 30, 1
			Controller.B2ssetcanplay 31, 1
			Controller.B2SSetGameOver 0
			Controller.B2SSetScore 4, hisc
		End If
		PUP1.state=1
		tilt=false
		state=true
		CanPlay1.state=1
		playsound "initialize" 
		players=1
		rst=0
		resettimer.enabled=true
	  else if state=true and players < 2 and Ballinplay=1 and (Credit>0 or freeplay=1) then
		if freeplay=0 then 
			credit=credit-1
			If credit < 1 Then DOF 122, DOFOff
			creditreel.setvalue(credit)
		end if
		players=players+1
		If B2SOn then
			Controller.B2ssetCredits Credit
			Controller.B2ssetcanplay 31, 2
		End If
		CanPlay2.state=1
		CanPlay1.state=0
		playsound "click" 
	   end if 
	  end if
	end if

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySoundat "plungerpull", plunger
	End If

	If keycode=LeftFlipperKey and State = false and OperatorMenu=0 then
		OperatorMenuTimer.Enabled = true
	end if

	If keycode=LeftFlipperKey and State = false and OperatorMenu=1 and Not OptionReelTimer.enabled and Not OptionsReelTimer.enabled then
		Options=Options+1
		If Options=7 then Options=1
		PlaySoundAt SoundFXDOF("reelclick",104,DOFPulse,DOFContactors), OptionReel
		OptionReelTimer.enabled=true
		Select Case (Options)
			Case 1:
				if Balls=3 then
					OptionsX=1
				  else
					OptionsX=2
				end if
			Case 2:
				if shadows=0 Then
					OptionsX=4    	'NO
				  Else
					OptionsX=3		'yes
				end If
			Case 3:
				if freeplay=0 Then
					OptionsX=4    	'no
				  Else
					OptionsX=3		'yes
				end if
			Case 4:
				Select Case (replays)
					Case 1:
						OptionsX=5		'Low
					Case 2:
						OptionsX=6		'Med
					Case 3:
						OptionsX=7		'High
				End Select
			Case 5:
				if clean=0 Then
					OptionsX=4    	'no
				  Else
					OptionsX=3		'yes
				end if
			Case 6:
				OptionsX=8
		End Select
		OptionsReelTimer.enabled=true
	end if

	If keycode=RightFlipperKey and State = false and OperatorMenu=1 and Not OptionReelTimer.enabled and Not OptionsReelTimer.enabled then
	  PlaySoundAt SoundFXDOF("reelclick",103,DOFPulse,DOFContactors), OptionsReel
	  Select Case (Options)
		Case 1:
			if Balls=3 then
				Balls=5
				InstCard.image="InstCard5balls"
				OptionsX=2
			  else
				Balls=3
				InstCard.image="InstCard3balls"
				OptionsX=1
			end if
			OptionsReelTimer.enabled=true
		Case 2:
			if shadows=0 Then
				shadows=1
				OptionsX=3    	'yes
			  Else
				shadows=0
				OptionsX=4		'no
			end If
			OptionsReelTimer.enabled=true
		Case 3:
			if freeplay=0 Then
				freeplay=1
				OptionsX=3    	'yes
			  Else
				freeplay=0
				OptionsX=4		'no
			end if
			OptionsReelTimer.enabled=true
		Case 4:
			Replays=Replays+1
			if Replays>3 then
				Replays=1
			end if
			Replay1=Replay1Table(Replays)
			Replay2=Replay2Table(Replays)
			replay3=Replay3Table(Replays)
			repcard.image = "replycard"&replays
			Select Case (replays)
				Case 1:
					OptionsX=5		'Low
				Case 2:
					OptionsX=6		'Med
				Case 3:
					OptionsX=7		'High
			End Select
			OptionsReelTimer.enabled=true
		Case 5:
			if clean=1 Then
				clean=0
				OptionsX=4		'no
				Wapron.image="GeminiApron"
				Arch.image="GeminiApron"
				PPlungerCover.image="Plungercover"
				Gemini.ColorGradeImage="ColorGradeLUT256x16_ModDeSat"
			  Else
				clean=1
				OptionsX=3    	'yes
				Wapron.image="CleanApron"
				Arch.image="CleanApron"
				PPlungerCover.image="Plungercover3"
				Gemini.ColorGradeImage="ColorGradeLUT256x16_ConSat"
			end if
			OptionsReelTimer.enabled=true
		Case 6:
			OperatorMenu=0
			savehs
			HideOptions
	  End Select
	End If

    If HSEnterMode Then HighScoreProcessKey(keycode)

  if tilt=false and state=true then
	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToEnd
        PlaySoundAtVol SoundFXDOF("fx_flipperup",101,DOFOn,DOFFlippers), LeftFlipper, 1
        PlaySoundAtVolLoops "Buzz", LeftFlipper, 0.01, -1
	End If
    
	If keycode = RightFlipperKey Then
		RightFlipper.RotateToEnd
        PlaySoundAtVol SoundFXDOF("fx_flipperup",102,DOFOn,DOFFlippers), RightFlipper, 1
        PlaySoundAtVolLoops "Buzz1", RightFlipper, 0.01, -1
	End If
    
	If keycode = LeftTiltKey Then
		Nudge 90, 2
		checktilt
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		checktilt
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		checktilt
	End If

	If keycode = MechanicalTilt Then
		gametilted
	End If

  end if  
End Sub

Sub OptionReelTimer_timer
	PlaySoundAt "fx_switch", OptionsReel
	if OptionReel.roty <> ((Options*60)-60) Then
		optionreel.roty=(OptionReel.roty+10) mod 360
	  Else
		me.enabled=False
	end If
end Sub

Sub OptionsReelTimer_timer
	PlaySoundAt "fx_switch", OptionsReel
	if OptionsReel.roty <> ((OptionsX*45)-45) Then
		OptionsReel.roty=(OptionsReel.roty+9) mod 360
	  Else
		me.enabled=False
	end If
end Sub

Sub DisplayOptions

	OptionReel.visible = True
	OptionsReel.visible = True
	if Balls=3 then
		OptionsReel.roty=0
	  else
		OptionsReel.roty=45
	end if
	OptionReel.roty=0
	OptionBox.visible = true
End Sub

Sub HideOptions
	for each objekt In OptionMenu
		objekt.visible = false
	next
End Sub

Sub OperatorMenuTimer_Timer
	OperatorMenu=1
	Displayoptions
	Options=1
End Sub

Sub Gemini_KeyUp(ByVal keycode)

'  **** ball control script
	if keycode = 203 then Cleft = 0' Left Arrow

	if keycode = 200 then Cup = 0' Up Arrow

	if keycode = 208 then Cdown = 0' Down Arrow

	if keycode = 205 then Cright = 0' Right Arrow
'  **** end ball control section

	If keycode = PlungerKey Then
		Plunger.Fire
		if ballhome.BallCntOver=1 then 
			PlaySoundAt "plunger", Plunger			'PLAY WHEN BALL IS HIT
		  Else
			PlaySoundAt "plungerfree", Plunger      'PLAY WHEN NO BALL TO PLUNGE
		end if
	End If

   	if keycode = LeftFlipperKey then
		OperatorMenuTimer.Enabled = false
	end if

   If tilt=false and state=true then
	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
        PlaySoundAtVol SoundFXDOF("fx_flipperdown",101,DOFOff,DOFFlippers), LeftFlipper, 1
		StopSound "Buzz"
	End If
    
	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
        PlaySoundAtVol SoundFXDOF("fx_flipperdown",102,DOFOff,DOFFlippers), RightFlipper, 1
		StopSound "Buzz1"
	End If
   End if
End Sub

'  **** ball control script

Sub StartControl_Hit()
	Set ControlBall = ActiveBall
	contballinplay = true
End Sub

Sub EndControl_Hit()
	contballinplay = false
End Sub

Dim Cup, Cdown, Cleft, Cright, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti

bcboost = 1 'Do Not Change - default setting
bcvel = 4 'Controls the speed of the ball movement
bcyveloffset = -0.01 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
	If Contball and ContBallInPlay then
		If Cright = 1 Then
			ControlBall.velx = bcvel*bcboost
		  ElseIf Cleft = 1 Then
			ControlBall.velx = - bcvel*bcboost
		  Else
			ControlBall.velx=0
		End If
		If Cup = 1 Then
			ControlBall.vely = -bcvel*bcboost
		  ElseIf Cdown = 1 Then
			ControlBall.vely = bcvel*bcboost
 		  Else
			ControlBall.vely= bcyveloffset
		End If
	End If
End Sub

'  **** end ball control script

sub flippertimer_timer()
	LFlip.RotY = LeftFlipper.CurrentAngle
	RFlip.RotY = RightFlipper.CurrentAngle
	Pgate.rotx = Gate.currentangle*.6



testbox.text=OptionReel.roty


end sub

Sub EBtimer_timer
	starstate=0
	if starred.state=1 then starstate=starstate+1
	if starwhite.state=1 then starstate=starstate+1
	if stargreen.state=1 then starstate=starstate+1
	if staryellow.state=1 then starstate=starstate+1
	if starstate=4 then ExtraBall.state=1
end sub

Sub PairedlampTimer_timer
	red2.state = red1.state
	white2.state = white1.state
	green2.state = green1.state
	yellow2.state = yellow1.state
	white3.state = white1.state
	green3.state = green1.state
	BumperLightA1.state=BumperLight1.state
	BumperLightA2.state=BumperLight2.state
end sub


sub resettimer_timer
    rst=rst+1
    scorereel1.resettozero 
    scorereel2.resettozero 

    If B2SOn then
		Controller.B2SSetScore 1, score(1)
		Controller.B2SSetScore 2, score(2)
		Controller.B2SSetData 3,0
		Controller.B2SSetData 4,0
	End If
    if rst=20 then
		newgame
		resettimer.enabled=false
    end if
end sub

Sub addcredit
      credit=credit+1
	  DOF 122, DOFOn
      if credit>15 then credit=15
	  creditreel.setvalue(credit)
	  If B2SOn Then Controller.B2ssetCredits Credit
End sub

Sub Drain_Hit()
	PlaySoundAt "drain", Drain
	DOF 108, DOFPulse
    me.timerenabled=1
End Sub

Sub Drain_timer
    scorebonus.enabled=true
    me.timerenabled=0
End Sub


sub ballhome_unhit
	DOF 128, DOFPulse
end sub

sub ballrel_hit
	shootagain.state=lightstateoff
end sub


sub scorebonus_timer            '************add bonus scoring if applicable
   if tilt=true then
        bonus=0
    Else
        if Bonusx3.state=1 then
            dbonus=3
          elseif Bonusx2.state=1 Then
			dbonus=2
		  Else
            dbonus=1
        End if
        if bonus>0 then
            if bonus>2 and dbonus=3 then
				chimescount=3
			  elseif bonus>1 or dbonus=2 then
                chimescount=2
              else
                chimescount=1
            end if
            chimestimer.interval=1
            chimestimer.enabled=true
            playsound "motor2", 0, 0.04
            me.enabled=False
          else
            bonus=0
            for each light in bonuslights: light.state=0: next
        end if
    end if
    if bonus=0 and chimestimer.enabled=0 then
         if shootagain.state=lightstateon and tilt=false then
              newball
              ballreltimer.enabled=true
            else
              if players=1 or player=2 then
                player=1
               Else
                player=player+1
              end if
              If B2SOn then Controller.B2ssetplayerup 30, player
              nextball
         end if
         me.enabled=false
    end if
End sub
 
sub chimestimer_timer
    chimestimer.interval=100
    addpoints 1000
 
    if (dbonus>1 and chimescount=1) or dbonus=1 then
        EVAL("Bonus"&bonus).state=0
        bonus=bonus-1
        if bonus >0 then
            EVAL("Bonus"&bonus).state=1
            if bonus=19 then bonus10.state=1
        end if
    end if
    chimescount = chimescount - 1
    if chimescount<1 then
        me.enabled=false
        ScoreBonus.enabled=True
    end if
end Sub

sub newgame
	for each light in bumperlights:light.state=1:next
	for each light in GIlights:light.state=1:next
	for i=1 to 2
		EVAL("Bumper"&i).hashitevent = 1
	Next
	LeftSlingShot.isdropped=False
	RightSlingShot.isdropped=False
	player=1
	PUP1.state=1
    score(1)=0
	score(2)=0
	If B2SOn then
		Controller.B2SSetScore 1, score(1)
		Controller.B2SSetScore 2, score(2)
	End If
    eg=0
    rep(1)=0
    rep(2)=0
	for each light in bonuslights:light.state=0:next
    special.state=0
    ExtraBall.state=0
	shootagain.state=lightstateoff
	newball
    gamov.text=" "
    tilttxt.text=" "
    If B2SOn then
		Controller.B2SSetScoreRollover 25, 0
		Controller.B2SSetScoreRollover 26, 0
		Controller.B2SSetGameOver 35,0
		Controller.B2SSetTilt 33,0
		Controller.B2SSetMatch 34,0
		Controller.B2ssetdata 11, 0
		Controller.B2ssetdata 7,0
		Controller.B2ssetdata 8,0
	End If
    biptxt.text="1"
	matchtxt.text=" "

	Drain.kick 60,45,0
    PlaySoundat SoundFXDOF("ballrelease",107,DOFPulse,DOFContactors), drain
end sub

sub newball
	for each light in colorlights:light.state=1:next
	for each light in starlights:light.state=0:next
    ExtraBall.state=0
	special.state=0
	DTReset.uservalue=-2
	DTReset.enabled=True
	if ballinplay=balls then bonusx2.state=1
End Sub


sub nextball
	for each light in bumperlights:light.state=1:next
	for each light in GIlights:light.state=1:next
	for i=1 to 2
		EVAL("Bumper"&i).hashitevent = 1
	Next
	LeftSlingShot.isdropped=False
	RightSlingShot.isdropped=False
	tilt=false
	tilttxt.text=" "
	If B2SOn then
		Controller.B2SSetTilt 33,0
		Controller.B2ssetdata 1, 1
	End If

	extraball.state=0
	bonusx2.state=0
	bonusx3.state=0
	special.state=0
	if player=1 then ballinplay=ballinplay+1
	if ballinplay>balls then
		playsound "endgame"
		eg=1
		ballreltimer.enabled=true
	  else
		if state=true and tilt=false then
		  newball
		  ballreltimer.enabled=true
		end if
		biptxt.text=ballinplay
		If B2SOn then Controller.B2ssetballinplay 32, Ballinplay
	end if
End Sub

sub ballreltimer_timer
	dim hiscstate
  if eg=1 then
	  hiscstate=0
      matchnum
	  turnoff
	  biptxt.text=" "
	  state=false
	  gamov.timerenabled=1
	  tilttxt.timerenabled=1
	  CanPlay1.state=0
	  CanPlay2.state=0
	  PUP1.state=0
	  PUP2.state=0
	  for i=1 to 2
		if score(i)>hisc then 
			hisc=score(i)
			hiscstate=1
		end if
		EVAL("Pup"&i).state=0
	  next
	  if hiscstate=1 then 
			HighScoreEntryInit()
			HStimer.uservalue = 0
			HStimer.enabled=1
	  end if
	  UpdatePostIt 
	  savehs
	  If B2SOn then 
        Controller.B2SSetGameOver 35,1
        Controller.B2ssetballinplay 32, 0
	    Controller.B2sStartAnimation "EOGame"
	    Controller.B2SSetScore 4, hisc
	    Controller.B2ssetcanplay 31, 0
	    Controller.B2ssetcanplay 30, 0
	  End If
	  LeftFlipper.RotateToStart
 	  StopSound "Buzz"
	  RightFlipper.RotateToStart
	  StopSound "Buzz1"
	  ballreltimer.enabled=false
  else
	Drain.kick 60,45,0
    PlaySoundAt SoundFXDOF("ballrelease",107,DOFPulse,DOFContactors), Drain
    ballreltimer.enabled=false
  end if
end sub

Sub HStimer_timer
	PlaySoundAtVol SoundFXDOF("knocker",138,DOFPulse,DOFKnocker), Plunger, 1
	DOF 139,DOFPulse
	HStimer.uservalue=HStimer.uservalue+1
	if HStimer.uservalue=3 then me.enabled=0
end sub

sub matchnum
	if matchnumb=0 then
		matchtxt.text="00"
	  else
		matchtxt.text=matchnumb*10
	end if
  If B2SOn then 
	If matchnumb=0 then 
			Controller.B2SSetMatch 34,100
		Else
			Controller.B2SSetMatch 34,Matchnumb*10
	end If
  end If
  For i=1 to players
    if (matchnumb*10)=(score(i) mod 100) then 
		if freeplay=0 then addcredit
		PlaySoundAt SoundFXDOF("knocker",121,DOFPulse,DOFKnocker), plunger
	end if
  next
end sub

'********** Bumpers

Sub Bumper1_Hit
   if tilt=false then
    PlaySoundAt SoundFXDOF("fx_bumper4",105, DOFPulse,DOFContactors), Bumper1
	if bumperlight1.state=1 then
		addscore bumperlitscore
	  else
		addscore bumperoffscore
	end if
   end if
End Sub


Sub Bumper2_Hit
   if tilt=false then
    PlaySoundAt SoundFXDOF("fx_bumper4",106, DOFPulse,DOFContactors), Bumper2
	if bumperlight2.state=1 then
		addscore bumperlitscore
	  else
		addscore bumperoffscore
	end if	
   end if
End Sub


sub FlashBumpers
	For each light in BumperLights
	  Light.State=0
	next
	FlashB.enabled=1
end sub

sub FlashB_timer
	For each light in BumperLights
	  Light.State=1
	next
	FlashB.enabled=0
end sub


'****** Color switches

Sub TRed1_Hit
   if tilt=false then
	DOF 109, DOFPulse
	flashbumpers
	If StarRed.State = 0 then StarRed.State  = 1
	Red1.state = 0
	If balls=3 then
		If StarYellow.State = 0 then StarYellow.State = 1
		Yellow1.state = 0
	end if
	addscore 500
	addbonus
   end if
End Sub

Sub TRed2_Hit
   if tilt=false then
	DOF 110, DOFPulse
	flashbumpers
	If StarRed.State = 0 then StarRed.State  = 1
	Red1.state = 0
	If balls=3 then
		If StarYellow.State = 0 then StarYellow.State = 1
		Yellow1.state = 0
	end if
	addscore 500
	addbonus
   end if
End Sub

Sub TWhite1_Hit
   if tilt=false then
	DOF 111, DOFPulse
	flashbumpers
	If StarWhite.State = 0 then StarWhite.State  = 1
	White1.state = 0
	addscore 500
	addbonus
   end if
End Sub

Sub TWhite2_Hit
   if tilt=false then
	DOF 112, DOFPulse
	flashbumpers
	If StarWhite.State = 0 then StarWhite.State  = 1
	White1.state = 0
	addscore 500
	addbonus
   end if
End Sub

Sub TWhite3_Hit
   if tilt=false then
	DOF 113, DOFPulse
	flashbumpers
	If StarWhite.State = 0 then StarWhite.State  = 1
	White1.state = 0
	addscore 500
	addbonus
   end if
End Sub

Sub TGreen1_Hit
   if tilt=false then
	DOF 114, DOFPulse
	flashbumpers
	If StarGreen.State = 0 then StarGreen.State  = 1
	Green1.state = 0
	addscore 500
	addbonus
   end if
End Sub

Sub TGreen2_Hit
   if tilt=false then
	DOF 115, DOFPulse
	flashbumpers
	If StarGreen.State = 0 then StarGreen.State  = 1
	Green1.state = 0
	addscore 500
	addbonus
   end if
End Sub

Sub TGreen3_Hit
   if tilt=false then
	DOF 116, DOFPulse
	flashbumpers
	If StarGreen.State = 0 then StarGreen.State  = 1
	Green1.state = 0
	addscore 500
	addbonus
   end if
End Sub

Sub TYellow1_Hit
   if tilt=false then
	DOF 117, DOFPulse
	flashbumpers
	If StarYellow.State = 0 then StarYellow.State  = 1
	Yellow1.state = 0
	If balls=3 then
		StarRed.State = 1
		Red1.state = 0
	end if
	addscore 500
	addbonus
   end if
End Sub

Sub TYellow2_Hit
   if tilt=false then
	DOF 118, DOFPulse
	flashbumpers
	If StarYellow.State = 0 then StarYellow.State  = 1
	Yellow1.state = 0
	If balls=3 then
		StarRed.State = 1
		Red1.state = 0
	end if
	addscore 500
	addbonus
   end if
End Sub

Sub TrigRout_Hit
	if tilt=false then
	  DOF 120, DOFPulse
	  flashbumpers
	  if special.state=1 then 
		PlaySoundAt SoundFXDOF("knocker",121,DOFPulse,DOFKnocker), Plunger
		addcredit
	  end if
	  addscore 5000
	  addbonus
	end if
End Sub

Sub TrigLout_Hit
	if tilt=false then
	  DOF 119, DOFPulse
	  flashbumpers
	  if extraball.state=1 then 
		shootagain.state=lightstateon
		extraball.state=0
	  end if
	  addscore 5000
	  addbonus
	end if
End Sub

'****Targets

Sub AwardCheck
	awardcount=0
	For j = 1 to 8
		if target(j).isdropped then awardcount=awardcount+1
	next

	if awardcount=8 then 
		if bonusx2.state=1 or bonusx3.state=1 then 
			bonusx3.state=1
			bonusx2.state=0
		  else
			bonusx2.state=1
		end if
		special.state=1
		DTReset.uservalue=0
		DTReset.enabled=True
	end if

End Sub

Sub DTreset_timer
    Select Case me.uservalue
        Case 1:
			playsoundat SoundFXDOF("bankreset",127,DOFPulse,DOFContactors), DTRedL
			for i=1 to 4
				target(i).isdropped=false
			next
		Case 4:
			playsoundat SoundFXDOF("bankreset",126,DOFPulse,DOFContactors), DTYellowR
			for i=5 to 8
				target(i).isdropped=false
			next
			me.enabled=False
    End Select
    me.uservalue = me.uservalue + 1
End Sub

Sub DTredL_dropped
	PlaySoundAt SoundFXDOF("target",123,DOFPulse,DOFContactors), DTRedL
	addscore (starstate+1)*1000
	If StarRed.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub


Sub DTredR_dropped
	PlaySoundAt SoundFXDOF("target",123,DOFPulse,DOFContactors), DTRedR
	addscore (starstate+1)*1000
	If StarRed.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub


Sub DTwhiteL_dropped
	PlaySoundAt SoundFXDOF("target",124,DOFPulse,DOFContactors), DTwhiteL
	addscore (starstate+1)*1000
	If StarWhite.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub

Sub DTwhiteR_dropped
	PlaySoundAt SoundFXDOF("target",124,DOFPulse,DOFContactors), DTwhiteR
	addscore (starstate+1)*1000
	If StarWhite.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub


Sub DTgreenL_dropped
	PlaySoundAt SoundFXDOF("target",125,DOFPulse,DOFContactors), DTGreenL
	addscore (starstate+1)*1000
	If StarGreen.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub


Sub DTGreenR_dropped
	PlaySoundAt SoundFXDOF("target",125,DOFPulse,DOFContactors), DTGreenR
	addscore (starstate+1)*1000
	If StarGreen.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub

Sub DTyellowL_dropped
	PlaySoundAt SoundFXDOF("target",126,DOFPulse,DOFContactors), DTYellowL
	addscore (starstate+1)*1000
	If StarYellow.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub

Sub DTyellowR_dropped
	PlaySoundAt SoundFXDOF("target",126,DOFPulse,DOFContactors), DTYellowR
	addscore (starstate+1)*1000
	If StarYellow.State = 1 then 
		addbonus
	end if
	AwardCheck
End Sub

sub addscore(points)
  if tilt=false then
    If Points < 100 and AddScore10Timer.enabled = false Then
        Add10 = Points \ 10
		matchnumb=matchnumb+1
		if matchnumb>9 then matchnumb=0
        AddScore10Timer.Enabled = TRUE
      ElseIf Points < 1000 and AddScore100Timer.enabled = false Then
        Add100 = Points \ 100
        AddScore100Timer.Enabled = TRUE
      ElseIf AddScore1000Timer.enabled = false Then
        Add1000 = Points \ 1000
        AddScore1000Timer.Enabled = TRUE
    End If
  end if
End Sub

Sub AddScore10Timer_Timer()
    if Add10 > 0 then
        AddPoints 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100 > 0 then
        AddPoints 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
        AddPoints 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddPoints(Points)			  ' Sounds: there are 3 sounds: tens, hundreds and thousands
    score(player)=score(player)+points
	sreels(player).addvalue(points)
	If B2SOn Then Controller.B2SSetScorePlayer player, score(player)

    If Points = 100 AND(Score(player) MOD 1000) \ 100 = 0 Then  'New 1000 reel
        PlaySoundAt SoundFXDOF("bell1000",133,DOFPulse,DOFChimes), chimesound
      ElseIf Points = 10 AND(Score(player) MOD 100) \ 10 = 0 Then 'New 100 reel
        PlaySoundAt SoundFXDOF("bell100",132,DOFPulse,DOFChimes), chimesound
      ElseIf points = 1000 Then
        PlaySoundAt SoundFXDOF("bell1000",133,DOFPulse,DOFChimes), chimesound
	  elseif Points = 100 Then
        PlaySoundAt SoundFXDOF("bell100",132,DOFPulse,DOFChimes), chimesound
	  Else
        PlaySoundAt SoundFXDOF("bell10",131,DOFPulse,DOFChimes), chimesound
    End If
	checkreplays
end sub

Sub checkreplays
    ' check replays and rollover
	if score(player)>99999 then
		If B2SOn then Controller.B2SSetScoreRollover 24 + player, 1
		EVAL("Reel100K"&player).setvalue(int(score(player)/100000))
	End if
    if score(player)=>replay1 and rep(player)=0 then
		addcredit
		rep(player)=1
		PlaySoundAt SoundFXDOF("knocker",121,DOFPulse,DOFKnocker), plunger
    end if
    if score(player)=>replay2 and rep(player)=1 then
		addcredit
		rep(player)=2
		PlaySoundAt SoundFXDOF("knocker",121,DOFPulse,DOFKnocker), plunger
    end if
    if score(player)=>replay3 and rep(player)=2 then
		addcredit
		rep(player)=3
		PlaySoundAt SoundFXDOF("knocker",121,DOFPulse,DOFKnocker), plunger
    end if
end sub 


Sub CheckTilt
	If Tilttimer.Enabled = True Then 
	 TiltSens = TiltSens + 1
	 if TiltSens = 3 Then GameTilted
	Else
	 TiltSens = 0
	 Tilttimer.Enabled = True
	End If
End Sub

Sub Tilttimer_Timer()
	Tilttimer.Enabled = False
End Sub

Sub GameTilted
	Tilt = True
	tilttxt.text="TILT"
    If B2SOn Then Controller.B2SSetTilt 33,1
    If B2SOn Then Controller.B2ssetdata 1, 0
	playsoundat "tilt", Plunger
	turnoff
End Sub

sub turnoff
	if tilt=true then
		for each light in bumperlights:light.state=0:next
		for each light in GIlights:light.state=0:next
	end if
	for i=1 to 2
		EVAL("Bumper"&i).hashitevent = 0
	Next
	LeftSlingShot.isdropped=true
	RightSlingShot.isdropped=true
	LeftFlipper.RotateToStart
	StopSound "Buzz"
	RightFlipper.RotateToStart
	StopSound "Buzz1"
end sub    


Sub addbonus
	bonus=bonus+1
	if bonus>19 then bonus=19
	if  bonus = 1 then 
		bonuslight(bonus).state=1
	  else
		bonuslight(bonus).state=1
		bonuslight(bonus-1).state=0
		if bonus>10 then bonuslight(10).state=1
	End if
End sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFXDOF("right_slingshot",103,DOFPulse,DOFContactors), slingR
	addscore 10
    RSling.Visible = 0
    RSling1.Visible = 1
	slingR.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:slingR.rotx = 10
        Case 4:	slingR.rotx = 0:RSLing2.Visible = 0:RSLing.Visible = 1:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFXDOF("left_slingshot",104,DOFPulse,DOFContactors), slingL
	addscore 10
    LSling.Visible = 0
    LSling1.Visible = 1
	slingL.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:slingL.rotx = 10
        Case 4:slingL.rotx = 0:LSLing2.Visible = 0:LSLing.Visible = 1:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'********** Dingwalls (scoring rubbers) - and/or animated Rubbers- timer 50

sub dingwalla_hit
	if state=true and tilt=false then addscore 10
	SlingA.visible=0
	SlingA1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwalla_timer									
	select case me.uservalue
		Case 1: SlingA1.visible=0: SlingA.visible=1
		case 2:	SlingA.visible=0: SlingA2.visible=1
		Case 3: SlingA2.visible=0: SlingA.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub


sub dingwallb_hit
	if state=true and tilt=false then addscore 10
	SlingB.visible=0
	SlingB1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallb_timer									
	select case me.uservalue
		Case 1: Slingb1.visible=0: SlingB.visible=1
		case 2:	SlingB.visible=0: Slingb2.visible=1
		Case 3: Slingb2.visible=0: SlingB.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub


sub dingwallc_hit
	if state=true and tilt=false then addscore 10
	Slingc.visible=0
	Slingc1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallc_timer									
	select case me.uservalue
		Case 1: Slingc1.visible=0: Slingc.visible=1
		case 2:	Slingc.visible=0: Slingc2.visible=1
		Case 3: Slingc2.visible=0: Slingc.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub dingwalld_hit
	if state=true and tilt=false then addscore 10
	Slingd.visible=0
	Slingd1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwalld_timer									
	select case me.uservalue
		Case 1: Slingd1.visible=0: Slingd.visible=1
		case 2:	Slingd.visible=0: Slingd2.visible=1
		Case 3: Slingd2.visible=0: Slingd.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub dingwalle_hit
	SlingE.visible=0
	Slinge1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwalle_timer									
	select case me.uservalue
		Case 1: Slinge1.visible=0: SlingE.visible=1
		case 2:	SlingE.visible=0: SlingE2.visible=1
		Case 3: Slinge2.visible=0: SlingE.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub dingwallf_hit
	if state=true and tilt=false then addscore 10
	slingF.visible=0
	Slingf1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallf_timer									
	select case me.uservalue
		Case 1: Slingf1.visible=0: slingF.visible=1
		case 2:	slingF.visible=0: Slingf2.visible=1
		Case 3: Slingf2.visible=0: slingF.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub dingwallg_hit
	if state=true and tilt=false then addscore 10
	slingG.visible=0
	SlingG1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallG_timer									
	select case me.uservalue
		Case 1: SlingG1.visible=0: slingG.visible=1
		case 2:	slingG.visible=0: SlingG2.visible=1
		Case 3: SlingG2.visible=0: slingG.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub dingwallh_hit
	Slingd.visible=0
	Slingh1.visible=1
	me.uservalue=1
	Me.timerenabled=1
end sub

sub dingwallh_timer									
	select case me.uservalue
		Case 1: Slingh1.visible=0: Slingd.visible=1
		case 2:	Slingd.visible=0: Slingh2.visible=1
		Case 3: Slingh2.visible=0: Slingd.visible=1: Me.timerenabled=0
	end Select
	me.uservalue=me.uservalue+1
end sub

sub savehs
    savevalue "Gemini", "credit", credit
    savevalue "Gemini", "hiscore", hisc
    savevalue "Gemini", "match", matchnumb
    savevalue "Gemini", "score1", score(1)
    savevalue "Gemini", "score2", score(2)
	savevalue "Gemini", "replays", replays
	savevalue "Gemini", "balls", balls
	savevalue "Gemini", "freeplay", freeplay
	savevalue "Gemini", "shadows", shadows
	savevalue "Gemini", "clean", clean
	savevalue "Gemini", "hsa1", HSA1
	savevalue "Gemini", "hsa2", HSA2
	savevalue "Gemini", "hsa3", HSA3
end sub

sub loadhs
    dim temp
	temp = LoadValue("Gemini", "credit")
    If (temp <> "") then credit = CDbl(temp)
    temp = LoadValue("Gemini", "hiscore")
    If (temp <> "") then hisc = CDbl(temp)
    temp = LoadValue("Gemini", "match")
    If (temp <> "") then matchnumb = CDbl(temp)
    temp = LoadValue("Gemini", "score1")
    If (temp <> "") then score(1) = CDbl(temp)
    temp = LoadValue("Gemini", "score2")
    If (temp <> "") then score(2) = CDbl(temp)
    temp = LoadValue("Gemini", "replays")
    If (temp <> "") then replays = CDbl(temp)
    temp = LoadValue("Gemini", "balls")
    If (temp <> "") then balls = CDbl(temp)
    temp = LoadValue("Gemini", "freeplay")
    If (temp <> "") then freeplay = CDbl(temp)
    temp = LoadValue("Gemini", "shadows")
    If (temp <> "") then shadows = CDbl(temp)
    temp = LoadValue("Gemini", "clean")
    If (temp <> "") then clean = CDbl(temp)
    temp = LoadValue("Gemini", "hsa1")
    If (temp <> "") then HSA1 = CDbl(temp)
    temp = LoadValue("Gemini", "hsa2")
    If (temp <> "") then HSA2 = CDbl(temp)
    temp = LoadValue("Gemini", "hsa3")
    If (temp <> "") then HSA3 = CDbl(temp)
end sub

Sub Gemini_Exit()
	savehs
	If B2SOn Then Controller.stop
End Sub

'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

'**************************************************************************
'                 Additional Positional Sound Playback Functions by DJRobX
'**************************************************************************

Sub PlaySoundAtVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set position at table object, vol, and loops manually.

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
		PlaySound sound, Loops, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / Gemini.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Gemini.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
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

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'			BALL SHADOW
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
        If BOT(b).X < Gemini.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/16) + ((BOT(b).X - (Gemini.Width/2))/17))' + 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/16) + ((BOT(b).X - (Gemini.Width/2))/17))' - 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
End Sub

'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub a_Triggers_Hit (idx)
	playsound "sensor", 0,1,AudioPan(ActiveBall),0,0,0,1,AudioFade(ActiveBall)
End sub

Sub a_Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_DropTargets_Hit (idx)
	PlaySound "DTDrop", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub a_Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub a_Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub a_Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub a_Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub a_Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub a_Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub


Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

'==========================================================================================================================================
'============================================================= START OF HIGH SCORES ROUTINES =============================================================
'==========================================================================================================================================
'
'ADD LINE TO TABLE_KEYDOWN SUB WITH THE FOLLOWING:    If HSEnterMode Then HighScoreProcessKey(keycode) AFTER THE STARTGAME ENTRY
'ADD: And Not HSEnterMode=true TO IF KEYCODE=STARTGAMEKEY
'TO SHOW THE SCORE ON POST-IT ADD LINE AT RELEVENT LOCATION THAT HAS:  UpdatePostIt
'TO INITIATE ADDING INITIALS ADD LINE AT RELEVENT LOCATION THAT HAS:  HighScoreEntryInit()
'ADD THE FOLLOWING LINES TO TABLE_INIT TO SETUP POSTIT
'	if HSA1="" then HSA1=25
'	if HSA2="" then HSA2=25
'	if HSA3="" then HSA3=25
'	UpdatePostIt
'ADD HSA1, HSA2 AND HSA3 TO SAVE AND LOAD VALUES FOR TABLE
'ADD A TIMER NAMED HighScoreFlashTimer WITH INTERVAL 100 TO TABLE
'SET HSSSCOREX BELOW TO WHATEVER VARIABLE YOU USE FOR HIGH SCORE.
'ADD OBJECTS TO PLAYFIELD (EASIEST TO JUST COPY FROM THIS TABLE)
'IMPORT POST-IT IMAGES


Dim HSA1, HSA2, HSA3
Dim HSEnterMode, hsLetterFlash, hsEnteredDigits(3), hsCurrentDigit, hsCurrentLetter
Dim HSArray  
Dim HSScoreM,HSScore100k, HSScore10k, HSScoreK, HSScore100, HSScore10, HSScore1, HSScorex	'Define 6 different score values for each reel to use
HSArray = Array("Postit0","postit1","postit2","postit3","postit4","postit5","postit6","postit7","postit8","postit9","postitBL","postitCM","Tape")
Const hsFlashDelay = 4

' ***********************************************************
'  HiScore DISPLAY 
' ***********************************************************

Sub UpdatePostIt
	dim tempscore
	HSScorex = hisc
	TempScore = HSScorex
	HSScore1 = 0
	HSScore10 = 0
	HSScore100 = 0
	HSScoreK = 0
	HSScore10k = 0
	HSScore100k = 0
	HSScoreM = 0
	if len(TempScore) > 0 Then
		HSScore1 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100 = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreK = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore10k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScore100k = cint(right(Tempscore,1))
	end If
	if len(TempScore) > 1 Then
		TempScore = Left(TempScore,len(TempScore)-1)
		HSScoreM = cint(right(Tempscore,1))
	end If
	Pscore6.image = HSArray(HSScoreM):If HSScorex<1000000 Then PScore6.image = HSArray(10)
	Pscore5.image = HSArray(HSScore100K):If HSScorex<100000 Then PScore5.image = HSArray(10)
	PScore4.image = HSArray(HSScore10K):If HSScorex<10000 Then PScore4.image = HSArray(10)
	PScore3.image = HSArray(HSScoreK):If HSScorex<1000 Then PScore3.image = HSArray(10)
	PScore2.image = HSArray(HSScore100):If HSScorex<100 Then PScore2.image = HSArray(10)
	PScore1.image = HSArray(HSScore10):If HSScorex<10 Then PScore1.image = HSArray(10)
	PScore0.image = HSArray(HSScore1):If HSScorex<1 Then PScore0.image = HSArray(10)
	if HSScorex<1000 then
		PComma.image = HSArray(10)
	else
		PComma.image = HSArray(11)
	end if
	if HSScorex<1000000 then
		PComma2.image = HSArray(10)
	else
		PComma2.image = HSArray(11)
	end if
'	if showhisc=1 and showhiscnames=1 then
'		for each objekt in hiscname:objekt.visible=1:next
		HSName1.image = ImgFromCode(HSA1, 1)
		HSName2.image = ImgFromCode(HSA2, 2)
		HSName3.image = ImgFromCode(HSA3, 3)
'	  else
'		for each objekt in hiscname:objekt.visible=0:next
'	end if
End Sub

Function ImgFromCode(code, digit)
	Dim Image
	if (HighScoreFlashTimer.Enabled = True and hsLetterFlash = 1 and digit = hsCurrentLetter) then
		Image = "postitBL"
	elseif (code + ASC("A") - 1) >= ASC("A") and (code + ASC("A") - 1) <= ASC("Z") then
		Image = "postit" & chr(code + ASC("A") - 1)
	elseif code = 27 Then
		Image = "PostitLT"
    elseif code = 0 Then
		image = "PostitSP"
    Else
      msgbox("Unknown display code: " & code)
	end if
	ImgFromCode = Image
End Function

Sub HighScoreEntryInit()
	HSA1=0:HSA2=0:HSA3=0
	HSEnterMode = True
	hsCurrentDigit = 0
	hsCurrentLetter = 1:HSA1=1
	HighScoreFlashTimer.Interval = 250
	HighScoreFlashTimer.Enabled = True
	hsLetterFlash = hsFlashDelay
End Sub

Sub HighScoreFlashTimer_Timer()
	hsLetterFlash = hsLetterFlash-1
	UpdatePostIt
	If hsLetterFlash=0 then 'switch back
		hsLetterFlash = hsFlashDelay
	end if
End Sub


' ***********************************************************
'  HiScore ENTER INITIALS 
' ***********************************************************

Sub HighScoreProcessKey(keycode)
    If keycode = LeftFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1-1:If HSA1=-1 Then HSA1=26 'no backspace on 1st digit
				UpdatePostIt
			Case 2:
				HSA2=HSA2-1:If HSA2=-1 Then HSA2=27
				UpdatePostIt
			Case 3:
				HSA3=HSA3-1:If HSA3=-1 Then HSA3=27
				UpdatePostIt
		 End Select
    End If

	If keycode = RightFlipperKey Then
		hsLetterFlash = hsFlashDelay
		Select Case hsCurrentLetter
			Case 1:
				HSA1=HSA1+1:If HSA1>26 Then HSA1=0
				UpdatePostIt
			Case 2:
				HSA2=HSA2+1:If HSA2>27 Then HSA2=0
				UpdatePostIt
			Case 3:
				HSA3=HSA3+1:If HSA3>27 Then HSA3=0
				UpdatePostIt
		 End Select
	End If
	
    If keycode = StartGameKey Then
		Select Case hsCurrentLetter
			Case 1:
				hsCurrentLetter=2 'ok to advance
				HSA2=HSA1 'start at same alphabet spot
'				EMReelHSName1.SetValue HSA1:EMReelHSName2.SetValue HSA2
			Case 2:
				If HSA2=27 Then 'bksp
					HSA2=0
					hsCurrentLetter=1
				Else
					hsCurrentLetter=3 'enter it
					HSA3=HSA2 'start at same alphabet spot
				End If
			Case 3:
				If HSA3=27 Then 'bksp
					HSA3=0
					hsCurrentLetter=2
				Else
					savehs 'enter it
					HighScoreFlashTimer.Enabled = False
					HSEnterMode = False
				End If
		End Select
		UpdatePostIt
    End If
End Sub


