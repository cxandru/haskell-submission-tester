#!/usr/bin/python3
#coding=utf-8

import os, subprocess, sys, resource, re
from tasty_xml2msg import xml_to_corrector_string
from os.path import join, isfile, isdir, basename, exists, islink
from shlex import quote
from shutil import copyfile
from glob import glob
from sys import argv,stderr,exit
import re

#new idea: since we are working with the maps we can make everything exercise-agnostic. This drastically improves reusability.


def gradeExcForSubmissionRetMaybeErr(exercise, submission, abs_path_to_exc, intermediate_dir, stack_project_root_path):
    '''grade the given exercise for the given submission by symlinking it to the stack_project_root_path,
    stack testing it there, and writing the results to a msg file with the name of
    the exercise in itermediate_dir/submission/. 
    Return an error_string if one occured, None otherwise.
    '''
    exc_name,subexc_name = exercise
    exercise_name = exc_name + subexc_name
    output = join(intermediate_dir, submission, exercise_name)
    xml_file = output+'.xml'
    msg_file = output+'.msg'
    
    # stack test the project where the test and reference Solution are in the test dir
    # while homework is in src. The allowed packages are specified in package.yaml.
    # src has ghc-option -XSafe.
    # process xml test output

    executed_target = join(stack_project_root_path, "src", "S.hs")
    #not strictly necessary with copyfile
    if isfile(executed_target) or islink(executed_target): os.remove(executed_target)
    #we copy bc stack doesn't like symlinks.
    copyfile(abs_path_to_exc, executed_target)

    #pushd
    prev_dir= os.getcwd()
    os.chdir(stack_project_root_path)

    
    try:
        suitename = "default" if not subexc_name else subexc_name
        waldlaufer_guards=["nice", "timeout", "--kill-after=0", "30", "prlimit", "--cpu=20", "--stack=500000"]
        cmd=["stack", "build", "--force-dirty", "--test", ':'+suitename, '--test-arguments="--xml='+ quote(xml_file)+'"']
        p = subprocess.run(waldlaufer_guards+cmd, stderr=subprocess.PIPE, check=True)
        with open(msg_file, 'w') as msg_fh:
             msg_fh.write(xml_to_corrector_string(xml_file, exc_name))
        return None
    except subprocess.CalledProcessError as e: #=nonzero exit code during compilition/execution
        if not isfile(xml_file): #compilation/execution error, not test failure
            err = extract_err(applyBckspcChars(e.stderr.decode('utf8')))
            with open(msg_file, 'w') as msg_fh:
                msg_fh.write(
                    exercise_name+": "+
                    err)
                return err
        else:
            with open(msg_file, 'w') as msg_fh:
                failure_string = xml_to_corrector_string(xml_file, exc_name)
                msg_fh.write(failure_string)
            return failure_string
    finally:
        os.chdir(prev_dir) #popd

def extract_err(stack_build_stderr_output):
    #the error may be either a normal compilation error in the submission
    #or it may result from the submission failing to implement a function tested
    #by the test file. Therefore the file producing the error may be either.
    #errors may be normal, parse, lexical etc.
    search_result = re.search(r'^(?:[^\n]+?)\.hs:\d+?:\d+?: error: ?(.+?)^Progress ',stack_build_stderr_output,flags=re.DOTALL | re.MULTILINE)
    if search_result:
        return "Compilation Error:\n"+search_result.group(1)
    elif re.search("exited with: ExitFailure \(-9\)",stack_build_stderr_output):
        return "CPU time or Memory limits exceeded. Probably bottomless recursion."
    else: return """The compilation error extraction regex didn't match on this output.
    Please report this error at https://gitlab.lrz.de/alexandru/haskell-submission-tester.
    This was the raw stderr: (Please attach this in your issue):
    """ + stack_build_stderr_output

#source (modified): https://stackoverflow.com/questions/36576216/apply-control-characters-to-a-string-python
def applyBckspcChars(input_string):
    """
    apply the backspace control character, otherwise output would contain ^H escape sequences. 
    The shell seems to do this at some point before piping output to a file.
    """
    # Initial state
    # String is stored as a list because
    # python forbids the modification of
    # a string
    displayed_string = [] 
    cursor_position = 0

    # Loop on our input (transitions sequence)
    for character in input_string:
        # Backward transition
        if character == "\x08":
            # Move the cursor backward
            cursor_position -= 1

        # Alphanumeric transition
        else:
            # Add the character to the string
            displayed_string[cursor_position:cursor_position+1] = character 
            # Move the cursor forward
            cursor_position += 1

    # We transform our "list" string back to a real string
    return "".join(displayed_string)
