# Multiparty Session Type Explorer

## Overview

Multiparty session types [1, 4] allow us to describe communication between a set
of participants in a protocol in what is known as a *global type*. Global types
can then be projected into a set of local types, which describe the
communication behaviour from the point of view of a particular participant.
Local types can then be used for typechecking or runtime monitoring.

Recent work [2] has investigated different behavioural properties on sets of
local types. For example:

  {
      X: Y ! hello(String) . End
      Y: X ? hello(Bool) . End
  }

is unsafe, since X tries to send Y a hello message with a `String` payload, but Y
tries to receive a hello message with a `Bool` payload.

On the other hand,

  {
      X: Y ! ping(String) . Y ? pong(Bool) . End
      Y: X ? ping(String) . X ! pong(Bool) . End
  }

is safe (since the payloads all match up); deadlock-free (since none of the
participants ever get stuck); and terminating (since all participants eventually
reach `End`).

The goal of this project is to design and implement an interface which allows
a user to parse in a set of local types, show each local type as a finite state
machine, and then allow a user to step through their execution.

## Objectives

  * Familiarise yourself with session types.
  * Parse local types into an abstract syntax tree.
  * Display each local type as a finite state machine.
  * Allow a user to step through the execution of the system.
  * (Extension) Integrate with a system such as mpstk [3] to display information
      about which properties a system satisfies.
  * (Extension) Investigate extensions, such as asynchrony or
      subtyping.

## References

[1] Kohei Honda, Nobuko Yoshida, Marco Carbone.
    Multiparty asynchronous session types. POPL 2008.

[2]	Alceste Scalas, Nobuko Yoshida:
    Less is more: multiparty session types revisited. POPL 2019.

[3] Alceste Scalas.
    mpstk - the MultiParty Session Types toolKit.
    https://github.com/alcestes/mpstk
 
[4] Nobuko Yoshida, Lorenzo Gheri.
    A Very Gentle Introduction to Multiparty Session Types. ICDCIT 2020.
