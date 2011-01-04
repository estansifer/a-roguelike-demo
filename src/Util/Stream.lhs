
> module Util.Stream {
>       Stream(),
>       make_stream,
>       is_ready,
>       block_until_ready,
>       next_value,
>       try_next_value
>   }

> import Control.Concurrent

> import Util.Util (loop)


Streams /must/ be used in a single-threaded manner.

> data Stream a = Stream {
>       values :: MVar a,
>       ready :: MVar ()
>   }

'make_stream' takes a getter and returns a stream that streams values from that
getter.  The getter is repeatedly run, with its output values placed into the stream.

> make_stream :: IO a -> IO (Stream a)
> make_stream get = do
>   values <- newEmptyMVar
>   ready <- newEmptyMVar
>   forkIO $ loop $ ((get >>= putMVar values) >> putMVar ready ())
>   return $ Stream {values = values, ready = ready}

'is_ready' returns whether or not a value is available in the stream.  It does
not block, and if it returns True then the next call to 'next_value' will
not block.

> is_ready :: Stream a -> IO Bool
> is_ready stream = do
>   m_ready <- tryTakeMVar (ready stream)
>   case m_ready of
>       Just _ -> do

A value is ready to be read from the stream.  Since 'ready' is full, it must
have been filled after filling 'values'.  Therefore 'values' is full.  Therefore
the putting half of the stream must be blocked by 'putMVar values', so it
cannot have re-filled 'ready' since we read from it.  Therefore the following
'putMVar ready' cannot block.

We re-fill 'ready' to restore the value we took from it.

>           putMVar (ready stream) ()
>           return True

>       Nothing -> return False

It is guaranteed that after this call returns, the next call to 'next_value'
will not block.

> block_until_ready :: Stream a -> IO ()
> block_until_ready stream = readMVar (ready stream)

'next_value' blocks until a value from the stream is ready.

'takeMVar (ready stream)' can only block if the putter is between
'putMVar values' and 'putMVar ready', which itself will not block.

> next_value :: Stream a -> IO a
> next_value stream = do
>   value <- takeMVar (values stream)
>   () <- takeMVar (ready stream)
>   return value

> try_next_value :: Stream a -> IO (Just a)
> try_next_value stream = do
>   m_value <- tryTakeMVar (values stream)
>   case m_value of
>       Just value -> takeMVar (ready stream) >> return (Just value)
>       Nothing -> return Nothing

This is done on a best-effort basis, there is no way to be sure that all values
pending have been dropped.

> drop_pending_values :: Stream a -> IO ()
> drop_pending_values stream = do
>   b <- is_ready stream
>   if not b then return () else do
>       next_value stream
>       threadDelay 100
>       drop_pending_values stream
