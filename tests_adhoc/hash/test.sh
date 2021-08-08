#!/bin/sh

[ $(futhark hash test.fut) = $(futhark hash ../hash/test.fut) ]
