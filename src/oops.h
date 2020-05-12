//
//  oops.h
//  Smalltalk-80
//
//  Created by Dan Banay on 3/5/20;
//  Copyright © 2020 Dan Banay; All rights reserved;
//

#pragma once

// Well known oops for various objects in an image

// initializeGuaranteedPointers  G&R pg. 576

// UndefinedObject and Booleans
static const int NilPointer = 2;
static const int FalsePointer = 4;
static const int TruePointer = 6;



// Root
static const int SchedulerAssociationPointer = 8;
static const int SmalltalkPointer = 25286; // SystemDictionary

// Classes

static const int ClassSmallInteger = 12;
static const int ClassStringPointer = 14;
static const int ClassArrayPointer = 16;
static const int ClassMethodContextPointer = 22;
static const int ClassBlockContextPointer = 24;
static const int ClassPointPointer = 26;
static const int ClassLargePositiveIntegerPointer = 28;
static const int ClassMessagePointer = 32;
static const int ClassCharacterPointer = 40;
static const int ClassCompiledMethod = 34;
static const int ClassSymbolPointer = 56;


static const int ClassFloatPointer = 20;

static const int ClassSemaphorePointer = 38;
static const int ClassDisplayScreenPointer = 834;
static const int ClassUndefinedObject = 25728; 

// Selectors
static const int DoesNotUnderstandSelector = 42;
static const int CannotReturnSelector = 44;
static const int MustBeBooleanSelector = 52;

// Tables
static const int SpecialSelectorsPointer = 48;
static const int CharacterTablePointer = 50;

/*
 dbanay - first oops 2..52 are special oops... see this from SystemTracer
 
 If using GC make sure these are roots
 specialObjects _
      "1:" (Array with: nil with: false with: true with: (Smalltalk associationAt: #Processor))
     , "5:" (Array with: Symbol table with: SmallInteger with: String with: Array)
     , "9:" (Array with: (Smalltalk associationAt: #Smalltalk) with: Float
                 with: MethodContext with: BlockContext)
     , "13:" (Array with: Point with: LargePositiveInteger with: DisplayBitmap with: Message)
     , "17:" (Array with: CompiledMethod with: #unusedOop18 with: Semaphore with: Character)
     , "21:" (Array with: #doesNotUnderstand: with: #cannotReturn:
                 with: #monitor: with: Smalltalk specialSelectors)
     , "25:" (Array with: Character characterTable with: #mustBeBoolean).
 specialObjects size = 26 ifFalse: [self error: 'try again!!'].
 */
