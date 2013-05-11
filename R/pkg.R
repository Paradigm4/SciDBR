#/*
#**
#* BEGIN_COPYRIGHT
#*
#* This file is part of SciDB.
#* Copyright (C) 2008-2013 SciDB, Inc.
#*
#* SciDB is free software: you can redistribute it and/or modify
#* it under the terms of the AFFERO GNU General Public License as published by
#* the Free Software Foundation.
#*
#* SciDB is distributed "AS-IS" AND WITHOUT ANY WARRANTY OF ANY KIND,
#* INCLUDING ANY IMPLIED WARRANTY OF MERCHANTABILITY,
#* NON-INFRINGEMENT, OR FITNESS FOR A PARTICULAR PURPOSE. See
#* the AFFERO GNU General Public License for the complete license terms.
#*
#* You should have received a copy of the AFFERO GNU General Public License
#* along with SciDB.  If not, see <http://www.gnu.org/licenses/agpl-3.0.html>
#*
#* END_COPYRIGHT
#*/

.onLoad = function(libname,pkgname)
{
# Maximum allowed sequential index limit (for larger, use between)
  options(scidb.index.sequence.limit=1000000)
# Default empty fill-in value
  options(scidb.default.value=NA)
# Maximum allowed elements in an array return result
  options(scidb.max.array.elements=100000000)
# Should we be indexing at one (native R) or at zero (native SciDB)
  options(scidb.index.at.one=TRUE)
}

.onUnload = function(libpath)
{
  options(scidb.index.sequence.limit=c())
  options(scidb.default.value=c())
  options(scidb.max.array.elements=c())
  options(scidb.index.at.one=c())
}

# scidb array object type map. We don't yet support strings in scidb array
# objects. Use df2scidb and iquery for strings.
# R type = SciDB type
.scidbtypes = list(
  double="double",
  integer="int32",
  logical="bool",
  character="char"
)

# SciDB Integer dimension minimum, maximum
.scidb_DIM_MIN = "-4611686018427387902"
.scidb_DIM_MAX = "4611686018427387903"
