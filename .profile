# Add GHC to PATH
export PATH="${HOME}/.local/bin:${HOME}/.cabal/bin/ghci/Contents/bin:${PATH}"
export PATH="${HOME}/.cabal/bin:${PATH}"

# Add Stack to PATH
export PATH="${HOME}/.local/bin:${PATH}"

# Add Brew to PATH
export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/Cellar:${PATH}"

# Add Clean-iTasks to PATH
export CLEAN_HOME=/usr/local/Cellar/clean-itasks/20160630
export PATH=$CLEAN_HOME:$CLEAN_HOME/bin:$CLEAN_HOME/lib/exe:$PATH

export ACCESS="abbottk@access.engr.oregonstate.edu${ACCESS}"
export FLIP="abbottk@flip.engr.oregonstate.edu${FLIP}"
export RABBIT="abbottk@rabbit.engr.oregonstate.edu${RABBIT}"

alias access='ssh $ACCESS'
alias flip='ssh $FLIP'
alias rabbit='ssh $RABBIT'

# added by Anaconda3 4.3.0 installer
export PATH="/Users/abbottk/anaconda/bin:$PATH"
