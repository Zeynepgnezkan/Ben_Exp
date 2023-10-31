#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2021.2.3),
    on Tue Jun 28 00:38:09 2022
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

from __future__ import absolute_import, division

from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '2021.2.3'
expName = 'Flanker'  # from the Builder filename that created this script
expInfo = {'participant': '', 'session': '001'}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Users/zeynepgunesozkan/Desktop/Dr. Angele/Flanker/Flanker_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

# Setup the Window
win = visual.Window(
    size=[1440, 900], fullscr=True, screen=0, 
    winType='pyglet', allowGUI=False, allowStencil=False,
    monitor='testMonitor', color='white', colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# Setup eyetracking
ioDevice = ioConfig = ioSession = ioServer = eyetracker = None

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard()

# Initialize components for Routine "Instr"
InstrClock = core.Clock()
instr_text = visual.TextStim(win=win, name='instr_text',
    text='Flanker deneyine hoşgeldiniz.\n\nBu deneyde,\nOrtadaki okun yönünü belirlemeniz gerekecek.\nDiğer tüm okları yok saymaya çalışın.\nOk solu gösteriyorsa A tuşuna basın.\nOk sağı gösteriyorsa K tuşuna basın.\nMümkün olduğunca hızlı ve doğru olmaya çalışın.\n\nDeneme turuna geçmek için BOŞLUK tuşuna basın.',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
instr_resp = keyboard.Keyboard()
win.mouseVisible = False


# Initialize components for Routine "fix"
fixClock = core.Clock()
polygon = visual.ShapeStim(
    win=win, name='polygon', vertices='cross',units='pix', 
    size=(20, 20),
    ori=0.0, pos=(0, 0),
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=0.0, interpolate=True)

# Initialize components for Routine "Practice"
PracticeClock = core.Clock()
arrow1_ = visual.ImageStim(
    win=win,
    name='arrow1_', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=0.0)
arrow2_ = visual.ImageStim(
    win=win,
    name='arrow2_', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-1.0)
arrow3_ = visual.ImageStim(
    win=win,
    name='arrow3_', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
target_p = visual.ImageStim(
    win=win,
    name='target_p', 
    image='sin', mask=None,
    ori=0.0, pos=(0, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-3.0)
arrow5_ = visual.ImageStim(
    win=win,
    name='arrow5_', 
    image='sin', mask=None,
    ori=0.0, pos=(0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-4.0)
arrow6_ = visual.ImageStim(
    win=win,
    name='arrow6_', 
    image='sin', mask=None,
    ori=0.0, pos=(0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-5.0)
arrow7_ = visual.ImageStim(
    win=win,
    name='arrow7_', 
    image='sin', mask=None,
    ori=0.0, pos=(0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-6.0)
key_resp_P = keyboard.Keyboard()
nCorr_p = 0


# Initialize components for Routine "FB"
FBClock = core.Clock()
corr = []
msg = ''
msgColor = []
text_2 = visual.TextStim(win=win, name='text_2',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "FB2"
FB2Clock = core.Clock()
text_3 = visual.TextStim(win=win, name='text_3',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "instr2"
instr2Clock = core.Clock()
text_4 = visual.TextStim(win=win, name='text_4',
    text='Deneme bitmiştir.\n\nDeneye geçmek için HERHANGİ bir tuşa basınız.\n',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
msg_exp = ''
key_resp_2 = keyboard.Keyboard()

# Initialize components for Routine "fix"
fixClock = core.Clock()
polygon = visual.ShapeStim(
    win=win, name='polygon', vertices='cross',units='pix', 
    size=(20, 20),
    ori=0.0, pos=(0, 0),
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=0.0, interpolate=True)

# Initialize components for Routine "trial"
trialClock = core.Clock()
arrow1 = visual.ImageStim(
    win=win,
    name='arrow1', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=0.0)
arrow2 = visual.ImageStim(
    win=win,
    name='arrow2', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-1.0)
arrow3 = visual.ImageStim(
    win=win,
    name='arrow3', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
target_t = visual.ImageStim(
    win=win,
    name='target_t', 
    image='sin', mask=None,
    ori=0.0, pos=(0, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-3.0)
arrow5 = visual.ImageStim(
    win=win,
    name='arrow5', 
    image='sin', mask=None,
    ori=0.0, pos=(0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-4.0)
arrow6 = visual.ImageStim(
    win=win,
    name='arrow6', 
    image='sin', mask=None,
    ori=0.0, pos=(0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-5.0)
arrow7 = visual.ImageStim(
    win=win,
    name='arrow7', 
    image='sin', mask=None,
    ori=0.0, pos=(0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-6.0)
key_resp = keyboard.Keyboard()

# Initialize components for Routine "instr3"
instr3Clock = core.Clock()
text_5 = visual.TextStim(win=win, name='text_5',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_3 = keyboard.Keyboard()

# Initialize components for Routine "reset"
resetClock = core.Clock()

# Initialize components for Routine "fix"
fixClock = core.Clock()
polygon = visual.ShapeStim(
    win=win, name='polygon', vertices='cross',units='pix', 
    size=(20, 20),
    ori=0.0, pos=(0, 0),
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=0.0, interpolate=True)

# Initialize components for Routine "trial"
trialClock = core.Clock()
arrow1 = visual.ImageStim(
    win=win,
    name='arrow1', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=0.0)
arrow2 = visual.ImageStim(
    win=win,
    name='arrow2', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-1.0)
arrow3 = visual.ImageStim(
    win=win,
    name='arrow3', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
target_t = visual.ImageStim(
    win=win,
    name='target_t', 
    image='sin', mask=None,
    ori=0.0, pos=(0, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-3.0)
arrow5 = visual.ImageStim(
    win=win,
    name='arrow5', 
    image='sin', mask=None,
    ori=0.0, pos=(0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-4.0)
arrow6 = visual.ImageStim(
    win=win,
    name='arrow6', 
    image='sin', mask=None,
    ori=0.0, pos=(0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-5.0)
arrow7 = visual.ImageStim(
    win=win,
    name='arrow7', 
    image='sin', mask=None,
    ori=0.0, pos=(0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-6.0)
key_resp = keyboard.Keyboard()

# Initialize components for Routine "instr3"
instr3Clock = core.Clock()
text_5 = visual.TextStim(win=win, name='text_5',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_3 = keyboard.Keyboard()

# Initialize components for Routine "reset"
resetClock = core.Clock()

# Initialize components for Routine "fix"
fixClock = core.Clock()
polygon = visual.ShapeStim(
    win=win, name='polygon', vertices='cross',units='pix', 
    size=(20, 20),
    ori=0.0, pos=(0, 0),
    lineWidth=1.0,     colorSpace='rgb',  lineColor='black', fillColor='black',
    opacity=None, depth=0.0, interpolate=True)

# Initialize components for Routine "trial"
trialClock = core.Clock()
arrow1 = visual.ImageStim(
    win=win,
    name='arrow1', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=0.0)
arrow2 = visual.ImageStim(
    win=win,
    name='arrow2', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-1.0)
arrow3 = visual.ImageStim(
    win=win,
    name='arrow3', 
    image='sin', mask=None,
    ori=0.0, pos=(-0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-2.0)
target_t = visual.ImageStim(
    win=win,
    name='target_t', 
    image='sin', mask=None,
    ori=0.0, pos=(0, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-3.0)
arrow5 = visual.ImageStim(
    win=win,
    name='arrow5', 
    image='sin', mask=None,
    ori=0.0, pos=(0.05, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-4.0)
arrow6 = visual.ImageStim(
    win=win,
    name='arrow6', 
    image='sin', mask=None,
    ori=0.0, pos=(0.1, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-5.0)
arrow7 = visual.ImageStim(
    win=win,
    name='arrow7', 
    image='sin', mask=None,
    ori=0.0, pos=(0.15, 0), size=(0.045, 0.03),
    color=[1,1,1], colorSpace='rgb', opacity=None,
    flipHoriz=False, flipVert=False,
    texRes=128.0, interpolate=True, depth=-6.0)
key_resp = keyboard.Keyboard()

# Initialize components for Routine "instr3"
instr3Clock = core.Clock()
text_5 = visual.TextStim(win=win, name='text_5',
    text='',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);
key_resp_3 = keyboard.Keyboard()

# Initialize components for Routine "THX"
THXClock = core.Clock()
text_6 = visual.TextStim(win=win, name='text_6',
    text='Flanker deneyinin sonuna geldiniz.\n\nKatılımınız için teşekkür ederiz.\n\nDeneyden çıkmak için BOŞLUK tuşuna basınız.',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='black', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "Instr"-------
continueRoutine = True
# update component parameters for each repeat
instr_resp.keys = []
instr_resp.rt = []
_instr_resp_allKeys = []
# keep track of which components have finished
InstrComponents = [instr_text, instr_resp]
for thisComponent in InstrComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
InstrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "Instr"-------
while continueRoutine:
    # get current time
    t = InstrClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=InstrClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *instr_text* updates
    if instr_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        instr_text.frameNStart = frameN  # exact frame index
        instr_text.tStart = t  # local t and not account for scr refresh
        instr_text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(instr_text, 'tStartRefresh')  # time at next scr refresh
        instr_text.setAutoDraw(True)
    
    # *instr_resp* updates
    waitOnFlip = False
    if instr_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        instr_resp.frameNStart = frameN  # exact frame index
        instr_resp.tStart = t  # local t and not account for scr refresh
        instr_resp.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(instr_resp, 'tStartRefresh')  # time at next scr refresh
        instr_resp.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(instr_resp.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(instr_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if instr_resp.status == STARTED and not waitOnFlip:
        theseKeys = instr_resp.getKeys(keyList=['space'], waitRelease=False)
        _instr_resp_allKeys.extend(theseKeys)
        if len(_instr_resp_allKeys):
            instr_resp.keys = _instr_resp_allKeys[-1].name  # just the last key pressed
            instr_resp.rt = _instr_resp_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in InstrComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "Instr"-------
for thisComponent in InstrComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('instr_text.started', instr_text.tStartRefresh)
thisExp.addData('instr_text.stopped', instr_text.tStopRefresh)
# check responses
if instr_resp.keys in ['', [], None]:  # No response was made
    instr_resp.keys = None
thisExp.addData('instr_resp.keys',instr_resp.keys)
if instr_resp.keys != None:  # we had a response
    thisExp.addData('instr_resp.rt', instr_resp.rt)
thisExp.nextEntry()
# the Routine "Instr" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
practice = data.TrialHandler(nReps=1.0, method='fullRandom', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('Flanker.xlsx'),
    seed=None, name='practice')
thisExp.addLoop(practice)  # add the loop to the experiment
thisPractice = practice.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisPractice.rgb)
if thisPractice != None:
    for paramName in thisPractice:
        exec('{} = thisPractice[paramName]'.format(paramName))

for thisPractice in practice:
    currentLoop = practice
    # abbreviate parameter names if possible (e.g. rgb = thisPractice.rgb)
    if thisPractice != None:
        for paramName in thisPractice:
            exec('{} = thisPractice[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "fix"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    # keep track of which components have finished
    fixComponents = [polygon]
    for thisComponent in fixComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    fixClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "fix"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = fixClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=fixClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *polygon* updates
        if polygon.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon.frameNStart = frameN  # exact frame index
            polygon.tStart = t  # local t and not account for scr refresh
            polygon.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon, 'tStartRefresh')  # time at next scr refresh
            polygon.setAutoDraw(True)
        if polygon.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon.tStartRefresh + 1-frameTolerance:
                # keep track of stop time/frame for later
                polygon.tStop = t  # not accounting for scr refresh
                polygon.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon, 'tStopRefresh')  # time at next scr refresh
                polygon.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in fixComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "fix"-------
    for thisComponent in fixComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    practice.addData('polygon.started', polygon.tStartRefresh)
    practice.addData('polygon.stopped', polygon.tStopRefresh)
    
    # ------Prepare to start Routine "Practice"-------
    continueRoutine = True
    # update component parameters for each repeat
    arrow1_.setImage(arrow_1)
    arrow2_.setImage(arrow_2)
    arrow3_.setImage(arrow_3)
    target_p.setImage(target)
    arrow5_.setImage(arrow_5)
    arrow6_.setImage(arrow_6)
    arrow7_.setImage(arrow_7)
    key_resp_P.keys = []
    key_resp_P.rt = []
    _key_resp_P_allKeys = []
    # keep track of which components have finished
    PracticeComponents = [arrow1_, arrow2_, arrow3_, target_p, arrow5_, arrow6_, arrow7_, key_resp_P]
    for thisComponent in PracticeComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    PracticeClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "Practice"-------
    while continueRoutine:
        # get current time
        t = PracticeClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=PracticeClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *arrow1_* updates
        if arrow1_.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow1_.frameNStart = frameN  # exact frame index
            arrow1_.tStart = t  # local t and not account for scr refresh
            arrow1_.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow1_, 'tStartRefresh')  # time at next scr refresh
            arrow1_.setAutoDraw(True)
        
        # *arrow2_* updates
        if arrow2_.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow2_.frameNStart = frameN  # exact frame index
            arrow2_.tStart = t  # local t and not account for scr refresh
            arrow2_.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow2_, 'tStartRefresh')  # time at next scr refresh
            arrow2_.setAutoDraw(True)
        
        # *arrow3_* updates
        if arrow3_.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow3_.frameNStart = frameN  # exact frame index
            arrow3_.tStart = t  # local t and not account for scr refresh
            arrow3_.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow3_, 'tStartRefresh')  # time at next scr refresh
            arrow3_.setAutoDraw(True)
        
        # *target_p* updates
        if target_p.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            target_p.frameNStart = frameN  # exact frame index
            target_p.tStart = t  # local t and not account for scr refresh
            target_p.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(target_p, 'tStartRefresh')  # time at next scr refresh
            target_p.setAutoDraw(True)
        
        # *arrow5_* updates
        if arrow5_.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow5_.frameNStart = frameN  # exact frame index
            arrow5_.tStart = t  # local t and not account for scr refresh
            arrow5_.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow5_, 'tStartRefresh')  # time at next scr refresh
            arrow5_.setAutoDraw(True)
        
        # *arrow6_* updates
        if arrow6_.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow6_.frameNStart = frameN  # exact frame index
            arrow6_.tStart = t  # local t and not account for scr refresh
            arrow6_.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow6_, 'tStartRefresh')  # time at next scr refresh
            arrow6_.setAutoDraw(True)
        
        # *arrow7_* updates
        if arrow7_.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow7_.frameNStart = frameN  # exact frame index
            arrow7_.tStart = t  # local t and not account for scr refresh
            arrow7_.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow7_, 'tStartRefresh')  # time at next scr refresh
            arrow7_.setAutoDraw(True)
        
        # *key_resp_P* updates
        waitOnFlip = False
        if key_resp_P.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp_P.frameNStart = frameN  # exact frame index
            key_resp_P.tStart = t  # local t and not account for scr refresh
            key_resp_P.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp_P, 'tStartRefresh')  # time at next scr refresh
            key_resp_P.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp_P.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp_P.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp_P.status == STARTED and not waitOnFlip:
            theseKeys = key_resp_P.getKeys(keyList=['a', 'k'], waitRelease=False)
            _key_resp_P_allKeys.extend(theseKeys)
            if len(_key_resp_P_allKeys):
                key_resp_P.keys = _key_resp_P_allKeys[-1].name  # just the last key pressed
                key_resp_P.rt = _key_resp_P_allKeys[-1].rt
                # was this correct?
                if (key_resp_P.keys == str(corrAns)) or (key_resp_P.keys == corrAns):
                    key_resp_P.corr = 1
                else:
                    key_resp_P.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in PracticeComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "Practice"-------
    for thisComponent in PracticeComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    practice.addData('arrow1_.started', arrow1_.tStartRefresh)
    practice.addData('arrow1_.stopped', arrow1_.tStopRefresh)
    practice.addData('arrow2_.started', arrow2_.tStartRefresh)
    practice.addData('arrow2_.stopped', arrow2_.tStopRefresh)
    practice.addData('arrow3_.started', arrow3_.tStartRefresh)
    practice.addData('arrow3_.stopped', arrow3_.tStopRefresh)
    practice.addData('target_p.started', target_p.tStartRefresh)
    practice.addData('target_p.stopped', target_p.tStopRefresh)
    practice.addData('arrow5_.started', arrow5_.tStartRefresh)
    practice.addData('arrow5_.stopped', arrow5_.tStopRefresh)
    practice.addData('arrow6_.started', arrow6_.tStartRefresh)
    practice.addData('arrow6_.stopped', arrow6_.tStopRefresh)
    practice.addData('arrow7_.started', arrow7_.tStartRefresh)
    practice.addData('arrow7_.stopped', arrow7_.tStopRefresh)
    # check responses
    if key_resp_P.keys in ['', [], None]:  # No response was made
        key_resp_P.keys = None
        # was no response the correct answer?!
        if str(corrAns).lower() == 'none':
           key_resp_P.corr = 1;  # correct non-response
        else:
           key_resp_P.corr = 0;  # failed to respond (incorrectly)
    # store data for practice (TrialHandler)
    practice.addData('key_resp_P.keys',key_resp_P.keys)
    practice.addData('key_resp_P.corr', key_resp_P.corr)
    if key_resp_P.keys != None:  # we had a response
        practice.addData('key_resp_P.rt', key_resp_P.rt)
    practice.addData('key_resp_P.started', key_resp_P.tStartRefresh)
    practice.addData('key_resp_P.stopped', key_resp_P.tStopRefresh)
    if key_resp_P.corr:
        nCorr_p = nCorr_p + 1
    # the Routine "Practice" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "FB"-------
    continueRoutine = True
    routineTimer.add(1.500000)
    # update component parameters for each repeat
    if key_resp_P.corr :
        msg = 'Doğru cevap'
        msgColor = 'green'
    else :
         msg = 'Yalnış cevap'
         msgColor = 'red'
    
    text_2.setColor(msgColor, colorSpace='rgb')
    text_2.setText(msg)
    # keep track of which components have finished
    FBComponents = [text_2]
    for thisComponent in FBComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    FBClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "FB"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = FBClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=FBClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_2* updates
        if text_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_2.frameNStart = frameN  # exact frame index
            text_2.tStart = t  # local t and not account for scr refresh
            text_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_2, 'tStartRefresh')  # time at next scr refresh
            text_2.setAutoDraw(True)
        if text_2.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > text_2.tStartRefresh + 1.5-frameTolerance:
                # keep track of stop time/frame for later
                text_2.tStop = t  # not accounting for scr refresh
                text_2.frameNStop = frameN  # exact frame index
                win.timeOnFlip(text_2, 'tStopRefresh')  # time at next scr refresh
                text_2.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in FBComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "FB"-------
    for thisComponent in FBComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    practice.addData('text_2.started', text_2.tStartRefresh)
    practice.addData('text_2.stopped', text_2.tStopRefresh)
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'practice'


# ------Prepare to start Routine "FB2"-------
continueRoutine = True
routineTimer.add(2.000000)
# update component parameters for each repeat
msg_f = '\n\n {} adet doğru cevap verdiniz. \n Ortalama cevap süreniz {:.3}ms.' .format(nCorr_p,key_resp_P.rt*1000)
text_3.setText(msg_f)
# keep track of which components have finished
FB2Components = [text_3]
for thisComponent in FB2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
FB2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "FB2"-------
while continueRoutine and routineTimer.getTime() > 0:
    # get current time
    t = FB2Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=FB2Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_3* updates
    if text_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_3.frameNStart = frameN  # exact frame index
        text_3.tStart = t  # local t and not account for scr refresh
        text_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_3, 'tStartRefresh')  # time at next scr refresh
        text_3.setAutoDraw(True)
    if text_3.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > text_3.tStartRefresh + 2-frameTolerance:
            # keep track of stop time/frame for later
            text_3.tStop = t  # not accounting for scr refresh
            text_3.frameNStop = frameN  # exact frame index
            win.timeOnFlip(text_3, 'tStopRefresh')  # time at next scr refresh
            text_3.setAutoDraw(False)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in FB2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "FB2"-------
for thisComponent in FB2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text_3.started', text_3.tStartRefresh)
thisExp.addData('text_3.stopped', text_3.tStopRefresh)

# ------Prepare to start Routine "instr2"-------
continueRoutine = True
# update component parameters for each repeat
nCorr_exp = 0


key_resp_2.keys = []
key_resp_2.rt = []
_key_resp_2_allKeys = []
# keep track of which components have finished
instr2Components = [text_4, key_resp_2]
for thisComponent in instr2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instr2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instr2"-------
while continueRoutine:
    # get current time
    t = instr2Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instr2Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_4* updates
    if text_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_4.frameNStart = frameN  # exact frame index
        text_4.tStart = t  # local t and not account for scr refresh
        text_4.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_4, 'tStartRefresh')  # time at next scr refresh
        text_4.setAutoDraw(True)
    
    # *key_resp_2* updates
    waitOnFlip = False
    if key_resp_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_2.frameNStart = frameN  # exact frame index
        key_resp_2.tStart = t  # local t and not account for scr refresh
        key_resp_2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_2, 'tStartRefresh')  # time at next scr refresh
        key_resp_2.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_2.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_2.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_2.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_2.getKeys(keyList=['space'], waitRelease=False)
        _key_resp_2_allKeys.extend(theseKeys)
        if len(_key_resp_2_allKeys):
            key_resp_2.keys = _key_resp_2_allKeys[-1].name  # just the last key pressed
            key_resp_2.rt = _key_resp_2_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instr2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instr2"-------
for thisComponent in instr2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text_4.started', text_4.tStartRefresh)
thisExp.addData('text_4.stopped', text_4.tStopRefresh)
# check responses
if key_resp_2.keys in ['', [], None]:  # No response was made
    key_resp_2.keys = None
thisExp.addData('key_resp_2.keys',key_resp_2.keys)
if key_resp_2.keys != None:  # we had a response
    thisExp.addData('key_resp_2.rt', key_resp_2.rt)
thisExp.addData('key_resp_2.started', key_resp_2.tStartRefresh)
thisExp.addData('key_resp_2.stopped', key_resp_2.tStopRefresh)
thisExp.nextEntry()
# the Routine "instr2" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1.0, method='fullRandom', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('Flanker.xlsx'),
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial:
        exec('{} = thisTrial[paramName]'.format(paramName))

for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "fix"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    # keep track of which components have finished
    fixComponents = [polygon]
    for thisComponent in fixComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    fixClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "fix"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = fixClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=fixClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *polygon* updates
        if polygon.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon.frameNStart = frameN  # exact frame index
            polygon.tStart = t  # local t and not account for scr refresh
            polygon.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon, 'tStartRefresh')  # time at next scr refresh
            polygon.setAutoDraw(True)
        if polygon.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon.tStartRefresh + 1-frameTolerance:
                # keep track of stop time/frame for later
                polygon.tStop = t  # not accounting for scr refresh
                polygon.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon, 'tStopRefresh')  # time at next scr refresh
                polygon.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in fixComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "fix"-------
    for thisComponent in fixComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('polygon.started', polygon.tStartRefresh)
    trials.addData('polygon.stopped', polygon.tStopRefresh)
    
    # ------Prepare to start Routine "trial"-------
    continueRoutine = True
    # update component parameters for each repeat
    arrow1.setImage(arrow_1)
    arrow2.setImage(arrow_2)
    arrow3.setImage(arrow_3)
    target_t.setImage(target)
    arrow5.setImage(arrow_5)
    arrow6.setImage(arrow_6)
    arrow7.setImage(arrow_7)
    key_resp.keys = []
    key_resp.rt = []
    _key_resp_allKeys = []
    # keep track of which components have finished
    trialComponents = [arrow1, arrow2, arrow3, target_t, arrow5, arrow6, arrow7, key_resp]
    for thisComponent in trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial"-------
    while continueRoutine:
        # get current time
        t = trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trialClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *arrow1* updates
        if arrow1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow1.frameNStart = frameN  # exact frame index
            arrow1.tStart = t  # local t and not account for scr refresh
            arrow1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow1, 'tStartRefresh')  # time at next scr refresh
            arrow1.setAutoDraw(True)
        
        # *arrow2* updates
        if arrow2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow2.frameNStart = frameN  # exact frame index
            arrow2.tStart = t  # local t and not account for scr refresh
            arrow2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow2, 'tStartRefresh')  # time at next scr refresh
            arrow2.setAutoDraw(True)
        
        # *arrow3* updates
        if arrow3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow3.frameNStart = frameN  # exact frame index
            arrow3.tStart = t  # local t and not account for scr refresh
            arrow3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow3, 'tStartRefresh')  # time at next scr refresh
            arrow3.setAutoDraw(True)
        
        # *target_t* updates
        if target_t.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            target_t.frameNStart = frameN  # exact frame index
            target_t.tStart = t  # local t and not account for scr refresh
            target_t.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(target_t, 'tStartRefresh')  # time at next scr refresh
            target_t.setAutoDraw(True)
        
        # *arrow5* updates
        if arrow5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow5.frameNStart = frameN  # exact frame index
            arrow5.tStart = t  # local t and not account for scr refresh
            arrow5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow5, 'tStartRefresh')  # time at next scr refresh
            arrow5.setAutoDraw(True)
        
        # *arrow6* updates
        if arrow6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow6.frameNStart = frameN  # exact frame index
            arrow6.tStart = t  # local t and not account for scr refresh
            arrow6.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow6, 'tStartRefresh')  # time at next scr refresh
            arrow6.setAutoDraw(True)
        
        # *arrow7* updates
        if arrow7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow7.frameNStart = frameN  # exact frame index
            arrow7.tStart = t  # local t and not account for scr refresh
            arrow7.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow7, 'tStartRefresh')  # time at next scr refresh
            arrow7.setAutoDraw(True)
        
        # *key_resp* updates
        waitOnFlip = False
        if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp.frameNStart = frameN  # exact frame index
            key_resp.tStart = t  # local t and not account for scr refresh
            key_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
            key_resp.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp.status == STARTED and not waitOnFlip:
            theseKeys = key_resp.getKeys(keyList=['a', 'k'], waitRelease=False)
            _key_resp_allKeys.extend(theseKeys)
            if len(_key_resp_allKeys):
                key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                key_resp.rt = _key_resp_allKeys[-1].rt
                # was this correct?
                if (key_resp.keys == str(corrAns)) or (key_resp.keys == corrAns):
                    key_resp.corr = 1
                else:
                    key_resp.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial"-------
    for thisComponent in trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('arrow1.started', arrow1.tStartRefresh)
    trials.addData('arrow1.stopped', arrow1.tStopRefresh)
    trials.addData('arrow2.started', arrow2.tStartRefresh)
    trials.addData('arrow2.stopped', arrow2.tStopRefresh)
    trials.addData('arrow3.started', arrow3.tStartRefresh)
    trials.addData('arrow3.stopped', arrow3.tStopRefresh)
    trials.addData('target_t.started', target_t.tStartRefresh)
    trials.addData('target_t.stopped', target_t.tStopRefresh)
    trials.addData('arrow5.started', arrow5.tStartRefresh)
    trials.addData('arrow5.stopped', arrow5.tStopRefresh)
    trials.addData('arrow6.started', arrow6.tStartRefresh)
    trials.addData('arrow6.stopped', arrow6.tStopRefresh)
    trials.addData('arrow7.started', arrow7.tStartRefresh)
    trials.addData('arrow7.stopped', arrow7.tStopRefresh)
    # check responses
    if key_resp.keys in ['', [], None]:  # No response was made
        key_resp.keys = None
        # was no response the correct answer?!
        if str(corrAns).lower() == 'none':
           key_resp.corr = 1;  # correct non-response
        else:
           key_resp.corr = 0;  # failed to respond (incorrectly)
    # store data for trials (TrialHandler)
    trials.addData('key_resp.keys',key_resp.keys)
    trials.addData('key_resp.corr', key_resp.corr)
    if key_resp.keys != None:  # we had a response
        trials.addData('key_resp.rt', key_resp.rt)
    trials.addData('key_resp.started', key_resp.tStartRefresh)
    trials.addData('key_resp.stopped', key_resp.tStopRefresh)
    if key_resp.corr:
        nCorr_exp = nCorr_exp + 1
    # the Routine "trial" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials'


# ------Prepare to start Routine "instr3"-------
continueRoutine = True
# update component parameters for each repeat
msg_exp = 'Biraz mola verebilirsiniz. \n\n {} adet doğru cevap verdiniz. \n Ortalama cevap süreniz {:.3}ms.\n\n Devam etmek için BOŞLUK tuşuna basınız.' .format(nCorr_exp,key_resp.rt*1000)
text_5.setText(msg_exp)
key_resp_3.keys = []
key_resp_3.rt = []
_key_resp_3_allKeys = []
# keep track of which components have finished
instr3Components = [text_5, key_resp_3]
for thisComponent in instr3Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instr3Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instr3"-------
while continueRoutine:
    # get current time
    t = instr3Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instr3Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_5* updates
    if text_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_5.frameNStart = frameN  # exact frame index
        text_5.tStart = t  # local t and not account for scr refresh
        text_5.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_5, 'tStartRefresh')  # time at next scr refresh
        text_5.setAutoDraw(True)
    
    # *key_resp_3* updates
    waitOnFlip = False
    if key_resp_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_3.frameNStart = frameN  # exact frame index
        key_resp_3.tStart = t  # local t and not account for scr refresh
        key_resp_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_3, 'tStartRefresh')  # time at next scr refresh
        key_resp_3.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_3.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_3.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_3.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_3.getKeys(keyList=['space'], waitRelease=False)
        _key_resp_3_allKeys.extend(theseKeys)
        if len(_key_resp_3_allKeys):
            key_resp_3.keys = _key_resp_3_allKeys[-1].name  # just the last key pressed
            key_resp_3.rt = _key_resp_3_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instr3Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instr3"-------
for thisComponent in instr3Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
msg_exp = ''
thisExp.addData('text_5.started', text_5.tStartRefresh)
thisExp.addData('text_5.stopped', text_5.tStopRefresh)
# check responses
if key_resp_3.keys in ['', [], None]:  # No response was made
    key_resp_3.keys = None
thisExp.addData('key_resp_3.keys',key_resp_3.keys)
if key_resp_3.keys != None:  # we had a response
    thisExp.addData('key_resp_3.rt', key_resp_3.rt)
thisExp.addData('key_resp_3.started', key_resp_3.tStartRefresh)
thisExp.addData('key_resp_3.stopped', key_resp_3.tStopRefresh)
thisExp.nextEntry()
# the Routine "instr3" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "reset"-------
continueRoutine = True
# update component parameters for each repeat
nCorr_exp = 0
# keep track of which components have finished
resetComponents = []
for thisComponent in resetComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
resetClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "reset"-------
while continueRoutine:
    # get current time
    t = resetClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=resetClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in resetComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "reset"-------
for thisComponent in resetComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "reset" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials_2 = data.TrialHandler(nReps=1.0, method='fullRandom', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('Flanker.xlsx'),
    seed=None, name='trials_2')
thisExp.addLoop(trials_2)  # add the loop to the experiment
thisTrial_2 = trials_2.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial_2.rgb)
if thisTrial_2 != None:
    for paramName in thisTrial_2:
        exec('{} = thisTrial_2[paramName]'.format(paramName))

for thisTrial_2 in trials_2:
    currentLoop = trials_2
    # abbreviate parameter names if possible (e.g. rgb = thisTrial_2.rgb)
    if thisTrial_2 != None:
        for paramName in thisTrial_2:
            exec('{} = thisTrial_2[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "fix"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    # keep track of which components have finished
    fixComponents = [polygon]
    for thisComponent in fixComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    fixClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "fix"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = fixClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=fixClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *polygon* updates
        if polygon.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon.frameNStart = frameN  # exact frame index
            polygon.tStart = t  # local t and not account for scr refresh
            polygon.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon, 'tStartRefresh')  # time at next scr refresh
            polygon.setAutoDraw(True)
        if polygon.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon.tStartRefresh + 1-frameTolerance:
                # keep track of stop time/frame for later
                polygon.tStop = t  # not accounting for scr refresh
                polygon.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon, 'tStopRefresh')  # time at next scr refresh
                polygon.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in fixComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "fix"-------
    for thisComponent in fixComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials_2.addData('polygon.started', polygon.tStartRefresh)
    trials_2.addData('polygon.stopped', polygon.tStopRefresh)
    
    # ------Prepare to start Routine "trial"-------
    continueRoutine = True
    # update component parameters for each repeat
    arrow1.setImage(arrow_1)
    arrow2.setImage(arrow_2)
    arrow3.setImage(arrow_3)
    target_t.setImage(target)
    arrow5.setImage(arrow_5)
    arrow6.setImage(arrow_6)
    arrow7.setImage(arrow_7)
    key_resp.keys = []
    key_resp.rt = []
    _key_resp_allKeys = []
    # keep track of which components have finished
    trialComponents = [arrow1, arrow2, arrow3, target_t, arrow5, arrow6, arrow7, key_resp]
    for thisComponent in trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial"-------
    while continueRoutine:
        # get current time
        t = trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trialClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *arrow1* updates
        if arrow1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow1.frameNStart = frameN  # exact frame index
            arrow1.tStart = t  # local t and not account for scr refresh
            arrow1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow1, 'tStartRefresh')  # time at next scr refresh
            arrow1.setAutoDraw(True)
        
        # *arrow2* updates
        if arrow2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow2.frameNStart = frameN  # exact frame index
            arrow2.tStart = t  # local t and not account for scr refresh
            arrow2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow2, 'tStartRefresh')  # time at next scr refresh
            arrow2.setAutoDraw(True)
        
        # *arrow3* updates
        if arrow3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow3.frameNStart = frameN  # exact frame index
            arrow3.tStart = t  # local t and not account for scr refresh
            arrow3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow3, 'tStartRefresh')  # time at next scr refresh
            arrow3.setAutoDraw(True)
        
        # *target_t* updates
        if target_t.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            target_t.frameNStart = frameN  # exact frame index
            target_t.tStart = t  # local t and not account for scr refresh
            target_t.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(target_t, 'tStartRefresh')  # time at next scr refresh
            target_t.setAutoDraw(True)
        
        # *arrow5* updates
        if arrow5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow5.frameNStart = frameN  # exact frame index
            arrow5.tStart = t  # local t and not account for scr refresh
            arrow5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow5, 'tStartRefresh')  # time at next scr refresh
            arrow5.setAutoDraw(True)
        
        # *arrow6* updates
        if arrow6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow6.frameNStart = frameN  # exact frame index
            arrow6.tStart = t  # local t and not account for scr refresh
            arrow6.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow6, 'tStartRefresh')  # time at next scr refresh
            arrow6.setAutoDraw(True)
        
        # *arrow7* updates
        if arrow7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow7.frameNStart = frameN  # exact frame index
            arrow7.tStart = t  # local t and not account for scr refresh
            arrow7.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow7, 'tStartRefresh')  # time at next scr refresh
            arrow7.setAutoDraw(True)
        
        # *key_resp* updates
        waitOnFlip = False
        if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp.frameNStart = frameN  # exact frame index
            key_resp.tStart = t  # local t and not account for scr refresh
            key_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
            key_resp.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp.status == STARTED and not waitOnFlip:
            theseKeys = key_resp.getKeys(keyList=['a', 'k'], waitRelease=False)
            _key_resp_allKeys.extend(theseKeys)
            if len(_key_resp_allKeys):
                key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                key_resp.rt = _key_resp_allKeys[-1].rt
                # was this correct?
                if (key_resp.keys == str(corrAns)) or (key_resp.keys == corrAns):
                    key_resp.corr = 1
                else:
                    key_resp.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial"-------
    for thisComponent in trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials_2.addData('arrow1.started', arrow1.tStartRefresh)
    trials_2.addData('arrow1.stopped', arrow1.tStopRefresh)
    trials_2.addData('arrow2.started', arrow2.tStartRefresh)
    trials_2.addData('arrow2.stopped', arrow2.tStopRefresh)
    trials_2.addData('arrow3.started', arrow3.tStartRefresh)
    trials_2.addData('arrow3.stopped', arrow3.tStopRefresh)
    trials_2.addData('target_t.started', target_t.tStartRefresh)
    trials_2.addData('target_t.stopped', target_t.tStopRefresh)
    trials_2.addData('arrow5.started', arrow5.tStartRefresh)
    trials_2.addData('arrow5.stopped', arrow5.tStopRefresh)
    trials_2.addData('arrow6.started', arrow6.tStartRefresh)
    trials_2.addData('arrow6.stopped', arrow6.tStopRefresh)
    trials_2.addData('arrow7.started', arrow7.tStartRefresh)
    trials_2.addData('arrow7.stopped', arrow7.tStopRefresh)
    # check responses
    if key_resp.keys in ['', [], None]:  # No response was made
        key_resp.keys = None
        # was no response the correct answer?!
        if str(corrAns).lower() == 'none':
           key_resp.corr = 1;  # correct non-response
        else:
           key_resp.corr = 0;  # failed to respond (incorrectly)
    # store data for trials_2 (TrialHandler)
    trials_2.addData('key_resp.keys',key_resp.keys)
    trials_2.addData('key_resp.corr', key_resp.corr)
    if key_resp.keys != None:  # we had a response
        trials_2.addData('key_resp.rt', key_resp.rt)
    trials_2.addData('key_resp.started', key_resp.tStartRefresh)
    trials_2.addData('key_resp.stopped', key_resp.tStopRefresh)
    if key_resp.corr:
        nCorr_exp = nCorr_exp + 1
    # the Routine "trial" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials_2'


# ------Prepare to start Routine "instr3"-------
continueRoutine = True
# update component parameters for each repeat
msg_exp = 'Biraz mola verebilirsiniz. \n\n {} adet doğru cevap verdiniz. \n Ortalama cevap süreniz {:.3}ms.\n\n Devam etmek için BOŞLUK tuşuna basınız.' .format(nCorr_exp,key_resp.rt*1000)
text_5.setText(msg_exp)
key_resp_3.keys = []
key_resp_3.rt = []
_key_resp_3_allKeys = []
# keep track of which components have finished
instr3Components = [text_5, key_resp_3]
for thisComponent in instr3Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instr3Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instr3"-------
while continueRoutine:
    # get current time
    t = instr3Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instr3Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_5* updates
    if text_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_5.frameNStart = frameN  # exact frame index
        text_5.tStart = t  # local t and not account for scr refresh
        text_5.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_5, 'tStartRefresh')  # time at next scr refresh
        text_5.setAutoDraw(True)
    
    # *key_resp_3* updates
    waitOnFlip = False
    if key_resp_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_3.frameNStart = frameN  # exact frame index
        key_resp_3.tStart = t  # local t and not account for scr refresh
        key_resp_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_3, 'tStartRefresh')  # time at next scr refresh
        key_resp_3.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_3.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_3.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_3.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_3.getKeys(keyList=['space'], waitRelease=False)
        _key_resp_3_allKeys.extend(theseKeys)
        if len(_key_resp_3_allKeys):
            key_resp_3.keys = _key_resp_3_allKeys[-1].name  # just the last key pressed
            key_resp_3.rt = _key_resp_3_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instr3Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instr3"-------
for thisComponent in instr3Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
msg_exp = ''
thisExp.addData('text_5.started', text_5.tStartRefresh)
thisExp.addData('text_5.stopped', text_5.tStopRefresh)
# check responses
if key_resp_3.keys in ['', [], None]:  # No response was made
    key_resp_3.keys = None
thisExp.addData('key_resp_3.keys',key_resp_3.keys)
if key_resp_3.keys != None:  # we had a response
    thisExp.addData('key_resp_3.rt', key_resp_3.rt)
thisExp.addData('key_resp_3.started', key_resp_3.tStartRefresh)
thisExp.addData('key_resp_3.stopped', key_resp_3.tStopRefresh)
thisExp.nextEntry()
# the Routine "instr3" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "reset"-------
continueRoutine = True
# update component parameters for each repeat
nCorr_exp = 0
# keep track of which components have finished
resetComponents = []
for thisComponent in resetComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
resetClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "reset"-------
while continueRoutine:
    # get current time
    t = resetClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=resetClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in resetComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "reset"-------
for thisComponent in resetComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# the Routine "reset" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials_3 = data.TrialHandler(nReps=1.0, method='fullRandom', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('Flanker.xlsx'),
    seed=None, name='trials_3')
thisExp.addLoop(trials_3)  # add the loop to the experiment
thisTrial_3 = trials_3.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial_3.rgb)
if thisTrial_3 != None:
    for paramName in thisTrial_3:
        exec('{} = thisTrial_3[paramName]'.format(paramName))

for thisTrial_3 in trials_3:
    currentLoop = trials_3
    # abbreviate parameter names if possible (e.g. rgb = thisTrial_3.rgb)
    if thisTrial_3 != None:
        for paramName in thisTrial_3:
            exec('{} = thisTrial_3[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "fix"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    # keep track of which components have finished
    fixComponents = [polygon]
    for thisComponent in fixComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    fixClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "fix"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = fixClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=fixClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *polygon* updates
        if polygon.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon.frameNStart = frameN  # exact frame index
            polygon.tStart = t  # local t and not account for scr refresh
            polygon.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon, 'tStartRefresh')  # time at next scr refresh
            polygon.setAutoDraw(True)
        if polygon.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon.tStartRefresh + 1-frameTolerance:
                # keep track of stop time/frame for later
                polygon.tStop = t  # not accounting for scr refresh
                polygon.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon, 'tStopRefresh')  # time at next scr refresh
                polygon.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in fixComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "fix"-------
    for thisComponent in fixComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials_3.addData('polygon.started', polygon.tStartRefresh)
    trials_3.addData('polygon.stopped', polygon.tStopRefresh)
    
    # ------Prepare to start Routine "trial"-------
    continueRoutine = True
    # update component parameters for each repeat
    arrow1.setImage(arrow_1)
    arrow2.setImage(arrow_2)
    arrow3.setImage(arrow_3)
    target_t.setImage(target)
    arrow5.setImage(arrow_5)
    arrow6.setImage(arrow_6)
    arrow7.setImage(arrow_7)
    key_resp.keys = []
    key_resp.rt = []
    _key_resp_allKeys = []
    # keep track of which components have finished
    trialComponents = [arrow1, arrow2, arrow3, target_t, arrow5, arrow6, arrow7, key_resp]
    for thisComponent in trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial"-------
    while continueRoutine:
        # get current time
        t = trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trialClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *arrow1* updates
        if arrow1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow1.frameNStart = frameN  # exact frame index
            arrow1.tStart = t  # local t and not account for scr refresh
            arrow1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow1, 'tStartRefresh')  # time at next scr refresh
            arrow1.setAutoDraw(True)
        
        # *arrow2* updates
        if arrow2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow2.frameNStart = frameN  # exact frame index
            arrow2.tStart = t  # local t and not account for scr refresh
            arrow2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow2, 'tStartRefresh')  # time at next scr refresh
            arrow2.setAutoDraw(True)
        
        # *arrow3* updates
        if arrow3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow3.frameNStart = frameN  # exact frame index
            arrow3.tStart = t  # local t and not account for scr refresh
            arrow3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow3, 'tStartRefresh')  # time at next scr refresh
            arrow3.setAutoDraw(True)
        
        # *target_t* updates
        if target_t.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            target_t.frameNStart = frameN  # exact frame index
            target_t.tStart = t  # local t and not account for scr refresh
            target_t.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(target_t, 'tStartRefresh')  # time at next scr refresh
            target_t.setAutoDraw(True)
        
        # *arrow5* updates
        if arrow5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow5.frameNStart = frameN  # exact frame index
            arrow5.tStart = t  # local t and not account for scr refresh
            arrow5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow5, 'tStartRefresh')  # time at next scr refresh
            arrow5.setAutoDraw(True)
        
        # *arrow6* updates
        if arrow6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow6.frameNStart = frameN  # exact frame index
            arrow6.tStart = t  # local t and not account for scr refresh
            arrow6.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow6, 'tStartRefresh')  # time at next scr refresh
            arrow6.setAutoDraw(True)
        
        # *arrow7* updates
        if arrow7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            arrow7.frameNStart = frameN  # exact frame index
            arrow7.tStart = t  # local t and not account for scr refresh
            arrow7.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(arrow7, 'tStartRefresh')  # time at next scr refresh
            arrow7.setAutoDraw(True)
        
        # *key_resp* updates
        waitOnFlip = False
        if key_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            key_resp.frameNStart = frameN  # exact frame index
            key_resp.tStart = t  # local t and not account for scr refresh
            key_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(key_resp, 'tStartRefresh')  # time at next scr refresh
            key_resp.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(key_resp.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(key_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if key_resp.status == STARTED and not waitOnFlip:
            theseKeys = key_resp.getKeys(keyList=['a', 'k'], waitRelease=False)
            _key_resp_allKeys.extend(theseKeys)
            if len(_key_resp_allKeys):
                key_resp.keys = _key_resp_allKeys[-1].name  # just the last key pressed
                key_resp.rt = _key_resp_allKeys[-1].rt
                # was this correct?
                if (key_resp.keys == str(corrAns)) or (key_resp.keys == corrAns):
                    key_resp.corr = 1
                else:
                    key_resp.corr = 0
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial"-------
    for thisComponent in trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials_3.addData('arrow1.started', arrow1.tStartRefresh)
    trials_3.addData('arrow1.stopped', arrow1.tStopRefresh)
    trials_3.addData('arrow2.started', arrow2.tStartRefresh)
    trials_3.addData('arrow2.stopped', arrow2.tStopRefresh)
    trials_3.addData('arrow3.started', arrow3.tStartRefresh)
    trials_3.addData('arrow3.stopped', arrow3.tStopRefresh)
    trials_3.addData('target_t.started', target_t.tStartRefresh)
    trials_3.addData('target_t.stopped', target_t.tStopRefresh)
    trials_3.addData('arrow5.started', arrow5.tStartRefresh)
    trials_3.addData('arrow5.stopped', arrow5.tStopRefresh)
    trials_3.addData('arrow6.started', arrow6.tStartRefresh)
    trials_3.addData('arrow6.stopped', arrow6.tStopRefresh)
    trials_3.addData('arrow7.started', arrow7.tStartRefresh)
    trials_3.addData('arrow7.stopped', arrow7.tStopRefresh)
    # check responses
    if key_resp.keys in ['', [], None]:  # No response was made
        key_resp.keys = None
        # was no response the correct answer?!
        if str(corrAns).lower() == 'none':
           key_resp.corr = 1;  # correct non-response
        else:
           key_resp.corr = 0;  # failed to respond (incorrectly)
    # store data for trials_3 (TrialHandler)
    trials_3.addData('key_resp.keys',key_resp.keys)
    trials_3.addData('key_resp.corr', key_resp.corr)
    if key_resp.keys != None:  # we had a response
        trials_3.addData('key_resp.rt', key_resp.rt)
    trials_3.addData('key_resp.started', key_resp.tStartRefresh)
    trials_3.addData('key_resp.stopped', key_resp.tStopRefresh)
    if key_resp.corr:
        nCorr_exp = nCorr_exp + 1
    # the Routine "trial" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials_3'


# ------Prepare to start Routine "instr3"-------
continueRoutine = True
# update component parameters for each repeat
msg_exp = 'Biraz mola verebilirsiniz. \n\n {} adet doğru cevap verdiniz. \n Ortalama cevap süreniz {:.3}ms.\n\n Devam etmek için BOŞLUK tuşuna basınız.' .format(nCorr_exp,key_resp.rt*1000)
text_5.setText(msg_exp)
key_resp_3.keys = []
key_resp_3.rt = []
_key_resp_3_allKeys = []
# keep track of which components have finished
instr3Components = [text_5, key_resp_3]
for thisComponent in instr3Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instr3Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instr3"-------
while continueRoutine:
    # get current time
    t = instr3Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instr3Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_5* updates
    if text_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_5.frameNStart = frameN  # exact frame index
        text_5.tStart = t  # local t and not account for scr refresh
        text_5.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_5, 'tStartRefresh')  # time at next scr refresh
        text_5.setAutoDraw(True)
    
    # *key_resp_3* updates
    waitOnFlip = False
    if key_resp_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        key_resp_3.frameNStart = frameN  # exact frame index
        key_resp_3.tStart = t  # local t and not account for scr refresh
        key_resp_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(key_resp_3, 'tStartRefresh')  # time at next scr refresh
        key_resp_3.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(key_resp_3.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(key_resp_3.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if key_resp_3.status == STARTED and not waitOnFlip:
        theseKeys = key_resp_3.getKeys(keyList=['space'], waitRelease=False)
        _key_resp_3_allKeys.extend(theseKeys)
        if len(_key_resp_3_allKeys):
            key_resp_3.keys = _key_resp_3_allKeys[-1].name  # just the last key pressed
            key_resp_3.rt = _key_resp_3_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instr3Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instr3"-------
for thisComponent in instr3Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
msg_exp = ''
thisExp.addData('text_5.started', text_5.tStartRefresh)
thisExp.addData('text_5.stopped', text_5.tStopRefresh)
# check responses
if key_resp_3.keys in ['', [], None]:  # No response was made
    key_resp_3.keys = None
thisExp.addData('key_resp_3.keys',key_resp_3.keys)
if key_resp_3.keys != None:  # we had a response
    thisExp.addData('key_resp_3.rt', key_resp_3.rt)
thisExp.addData('key_resp_3.started', key_resp_3.tStartRefresh)
thisExp.addData('key_resp_3.stopped', key_resp_3.tStopRefresh)
thisExp.nextEntry()
# the Routine "instr3" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "THX"-------
continueRoutine = True
# update component parameters for each repeat
# keep track of which components have finished
THXComponents = [text_6]
for thisComponent in THXComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
THXClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "THX"-------
while continueRoutine:
    # get current time
    t = THXClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=THXClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text_6* updates
    if text_6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text_6.frameNStart = frameN  # exact frame index
        text_6.tStart = t  # local t and not account for scr refresh
        text_6.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text_6, 'tStartRefresh')  # time at next scr refresh
        text_6.setAutoDraw(True)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in THXComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "THX"-------
for thisComponent in THXComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text_6.started', text_6.tStartRefresh)
thisExp.addData('text_6.stopped', text_6.tStopRefresh)
# the Routine "THX" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='auto')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
