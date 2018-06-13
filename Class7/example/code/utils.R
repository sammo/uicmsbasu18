PrepareDataFrameForReduction = function(df, resp.var = NA, row.id = NA,
																				split.size = 5E3, id.name = TRUE,
																				var.names = {}, scale = TRUE,
																				cat.min = 0) {
	# Prepares a data.frame for matrix reduction.  This function will remove the
	#  respones variable (optional) and assign unique row names (optional).
	#  Categorical features (either with "Id" in their name or given) will be
	#  expanded out into binary columns with their respective values appended to
	#  the column name.  Numerical features will be scaled (optional)
	#
	# Input:
	#  df										data frame containing numerical and categorical
	#												columns.
	#
	#  resp.var			NA			the response variable of the data.
	#
	#  row.id				NA			unique row identifier.
	#
	#  split.size		5E3			for time efficenicy, the data frame is split into this
	#												size of subsets.  1E3 is stable, but can be changed.
	#
	#  id.name			TRUE		if the categorical features all have "ID" in the name.
	#
	#  var.names		{}			if id.name = FALSE, this needs to be the categorical
	#                       feature column names (not indicies)
	#
	#  scale				TRUE		if the numerical features shoulded be scaled
	#
	# Output:
	#  df.final							data frame with categorical data expanded into binary
	#												columns and named accordingly.

	# initialize
	library(plyr)
	df.final.temp = list()

	# account for response or row id
	if(!is.na(row.id[1])) {
		row.id.ind   = which(names(df) == row.id)
		rownames(df) = df[, row.id.ind]
		df           = df[, -row.id.ind]
	}
	if(!is.na(resp.var[1])) {
		df           = df[, -which(names(df) == resp.var)]
	}

	# get categorical features
	if (id.name) {
		remove.temp = which(grepl("ID", names(df)))
	} else {
		remove.temp = which(names(df) %in% var.names)
	}

	# reduce categorical features
	if(cat.min > 0) {
		for (i in 1:length(remove.temp)) {
			col.temp = df[, remove.temp[i]]
			tab.temp = table(col.temp)
			low.temp = names(tab.temp)[which(as.vector(tab.temp) < cat.min)]
			col.temp[which(col.temp %in% low.temp)] = -2
			df[, remove.temp[i]] = col.temp
		}
	}


	# split matrix for speed
	df.row = nrow(df)
	if (df.row < split.size) {
		split.n      = 1
		split.len    = df.row
	} else {
		split.n      = ceiling(df.row/split.size)
		split.len    = round(df.row/split.n)
	}

	# iterate through splits
	for (i in 1:split.n) {
		# set up indices
		start = 1 + split.len*(i-1)
		if (i < split.n) {
			end = split.len*i
		} else {
			end = nrow(df)
		}

		# get temp df
		df.temp = df[start:end, ]

		# seperate out categorical data
		df.factor = as.data.frame(apply(df.temp[, remove.temp], 2, as.factor))
		names(df.factor) = paste0(names(df.factor), "_")

		# remove out single factors
		ind.single       = as.vector(which(lapply(apply(df.factor, 2, unique),
																							length) == 1))
		df.factor.single = df.factor[, ind.single, drop=FALSE]

		# if we have single factors, split them out manually
		if (length(ind.single) != 0) {
			val.single              = apply(df.factor.single, 2, unique)
			names(df.factor.single) = paste0(names(val.single), as.vector(val.single))
			df.factor.single[, ]    = 1
			df.factor               = df.factor[, -ind.single]
		}

		# split out non-single factors and combine with single factors
		if (dim(df.factor)[2] != 0) {
			# expand out factors
			df.factor.binary = model.matrix(~ . + 0, data=df.factor, contrasts.arg =
																				lapply(df.factor, contrasts,
																							 contrasts=FALSE))
			df.factor = as.data.frame(df.factor.binary)
			if(dim(df.factor.single)[2] != 0) {
				df.factor = cbind(df.factor, df.factor.single)
			}
		} else {
			df.factor = df.factor.single
		}

		# reestablish factors as numeric
		df.factor = apply(df.factor, 2, as.numeric)

		# seperate out non-factors
		df.numeric = df.temp[, -as.numeric(remove.temp)]
		df.numeric = as.data.frame(apply(df.numeric, 2, as.numeric))

		# scale data if needed
		if (scale) {
			df.numeric = as.data.frame(scale(df.numeric))
		}

		# finalize matrix
		df.final.temp[[i]] = cbind(df.numeric, df.factor)
	}

	# finalize
	df.final = ldply(df.final.temp)
	df.final[is.na(df.final)] = 0
	rownames(df.final) = rownames(df)

	# return
	return(df.final)
}

GetReducedMatrix = function(df, method = "ICA", k = 100, split.size = 5E4) {
	# Purpose of this function is to reduce our matrix to a fixed dimension (k).
	# Remember that arbitrarly choosing columns is inefficient so we first
	# need to apply an algorithm that find the most "interesting" dimensions.
	#
	# Args:
	#   df      (data.frame): the matrix
	#   method      (string): choice of ICA or rSVD
	#   k          (integer): desired number of dimensions
	#   split.size (integer): for computational efficenicy, split large matricies
	#
	# Returns:
	#   df.final (data.frame): the reduced data.frame with k dimensions

	# initialize
	library(plyr)
	df.final.temp = list()
	if(method == "ICA") {
		library(fastICA)
	} else if(method == "rSVD") {
		library(rsvd)
	}

	# split matrix for speed
	df.row = nrow(df)
	if (df.row < split.size) {
		split.n      = 1
		split.len    = df.row
	} else {
		split.n      = floor(df.row/split.size)
		split.len    = round(df.row/split.n)
	}

	# setup data
	df = df[sample(nrow(df)), ]

	# go through splits
	pb = txtProgressBar(min = 0, max = 1, style = 3)
	for (i in 1:split.n) {

		# set up indices
		start = 1 + split.len*(i-1)
		if (i < split.n) {
			end = split.len*i
		} else {
			end = nrow(df)
		}

		# get temp df
		df.temp = df[start:end, ]

		if(method == "ICA") {
			df.red.temp = fastICA(as.matrix(df.temp), n.comp=k, alg.typ="parallel",
														method="C")
			df.final.temp[[i]] = as.data.frame(df.red.temp$S)
		} else if (method == "rSVD") {

		}

		# clean up
		rm(df.temp, df.red.temp)
		gc()
		setTxtProgressBar(pb, i/split.n)
	}
	close(pb)

	# return
	df.final = ldply(df.final.temp)
	rownames(df.final) = rownames(df)
	return(df.final)
}

GetDistance = function(df, row.name, type='euclidean') {
  # For a df (assumed to be row vectors) find the distance from the target
  #  row to all other rows.  Distance based on "type" input var.
  #
  # Args:
  #   df   (data.frame): a df where each row is a vector
  #   row.name (string): row.name in df to compare to the rest
  #   type     (string): which distance metric to use
  #
  # Returns:
  #   dis.matrix (data.frame): row.names are which vector is compared to the
  #                             row.name vector.  Value is actual distance.

  # Make sure the row.name is valid
  if (row.name %in% rownames(df)) {
    row.ind = which(rownames(df) == row.name)
    vec = df[row.ind, ]
    df = df[-row.ind, ]
  } else {
    return('invalid row.name')
  }

  # loop over rows, getting distance per row.
  # NOTE: this code is FAR from production quality, I wrote this quickly as
  #  an example.  Think of how it could be improved.
  for (row in rownames(df)) {
    temp.vec = df[row, ]
    if (type == 'euclidean') {
      df.dist[row, 'dist'] = as.numeric(dist(rbind(vec, temp.vec)))
    } else if (type == 'cosine') {
      df.dist[row, 'dist'] = sum(vec*temp.vec)/sqrt(sum(vec^2)*sum(temp.vec^2))
    }
  }

  return(df.dist)
}
