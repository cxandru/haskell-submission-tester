#!/usr/bin/python3
#coding=utf-8

import os, subprocess, re, codecs
from tasty_xml2msg import xml_to_corrector_string
from os.path import join, isfile, isdir, basename, exists
from shlex import quote
from glob import glob
from sys import argv,stderr,exit

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

    #We symlink the submission to the stack dir. Use subprocess in order to replace symlink if already there. This might not really be necessary...
    os.system("ln -snf {hw_exc} {stack_dest}".format(
        hw_exc = quote(abs_path_to_exc),
        stack_dest = quote(join(stack_project_root_path, 'src', 'S.hs'))
        )
    )
    prev_dir= os.getcwd()
    os.chdir(stack_project_root_path)
    try:
        result=subprocess.check_output( """
        timeout --kill-after=20 60 stack build --test :{suitename} --test-arguments="--xml {xml_file}"
        """.format(
            abs_path_to_exc = abs_path_to_exc
            , xml_file = quote(xml_file)
            , msg_file = quote(msg_file)
            , suitename = "default" if not subexc_name else subexc_name 
        )
                                        , shell=True
                                        , stderr=subprocess.STDOUT
                                        , timeout=80 )
        with open(msg_file, 'w') as msg_fh:
            msg_fh.write(xml_to_corrector_string(xml_file, exc_name))
        return None
    except subprocess.CalledProcessError as e: #=nonzero exit code during compilation
        err = prettify_err_msg(e.output.decode("utf-8"), intermediate_dir)    
        with open(msg_file, 'w') as msg_fh:
            msg_fh.write('{}: does not compile: {}'.format(exercise_name),
                    err )
        return err
    except subprocess.TimeoutExpired:
        with open(msg_file, 'w') as msg_fh:
            msg_fh.write('{}: times out. Probably bottomless recursion.'.format(exercise_name))
        return "timeout"
    finally:
        os.chdir(prev_dir) #popd

def prettify_err_msg(err, intermediate_dir):
    """
    Not exactly sure if this is necessary. It seems mostly to get rid of compiler suggestions?
    """
    err = err.replace('compilation IS NOT required\n','').replace('\n\n', '\n')
    err = err.replace(intermediate_dir, '')
    err = "\n  ".join([e for e in err.split('\n', 6)[:6] if not e[:23]=='    Perhaps you meant `']) + "\n" # take only first 5lines or error msg and strip one-line suggestions
    err = err.split("      Perhaps you meant one of these:", 1)[0] # strip also multiline suggestions
    return err
    

