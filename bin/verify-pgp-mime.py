#!/usr/bin/python

import email
import os
import sys

with open(sys.argv[1]) as mfile: message = email.message_from_file(mfile)

if message.get_content_type() != "multipart/signed":
    raise SystemExit, "message not signed"

# Really, you'd want safe temporary filenames, and you'd want to clean them up
with open("msg", "wb") as msg: msg.write(message.get_payload(0).as_string())
with open("sig", "wb") as sig: sig.write(message.get_payload(1).get_payload())

# Delegate the real business to gpg
os.execvp("gpg", ["gpg", "--verify", "sig", "msg"])
