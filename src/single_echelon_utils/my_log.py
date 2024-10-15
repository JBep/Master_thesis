import logging

# Setting up logger
def setup_logging(name="logger",
                  filepath=None,
                  filemode='w',
                  stream_log_level="DEBUG",
                  file_log_level="DEBUG"):
    
    logger = logging.getLogger(name)
    logger.setLevel("DEBUG")
    formatter = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    ch = logging.StreamHandler()
    ch.setLevel(getattr(logging, stream_log_level))
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    
    if filepath is not None:
        fh = logging.FileHandler(filepath, mode = filemode)
        fh.setLevel(getattr(logging, file_log_level))
        fh.setFormatter(formatter)
        logger.addHandler(fh)
    return logger

logger_default = setup_logging(name="default_log",filepath="logger.log")
logger_sparse = setup_logging(name="sparse_log",filepath="logger_sparse.log")

def log_output(log_name):
    def log_this(function):
        logger = logging.getLogger(log_name)
        def new_function(*args,**kwargs):
            logger.debug(f"{function.__name__}")
            output = function(*args,**kwargs)
            logger.debug(f"{function.__name__} returned: {output}")
            return output
        return new_function
    return log_this

def log_input_and_output(log_name):
    def log_this(function):
        logger = logging.getLogger(log_name)
        def new_function(*args,**kwargs):
            logger.debug(f"{function.__name__} - {args} - {kwargs}")
            output = function(*args,**kwargs)
            logger.debug(f"{function.__name__} returned: {output}")
            return output
        return new_function
    return log_this

def log(log_name):
    def log_this(function):
        logger = logging.getLogger(log_name)
        def new_function(*args,**kwargs):
            logger.debug(f"{function.__name__}")
            output = function(*args,**kwargs)
            logger.debug(f"{function.__name__} ended.")
            return output
        return new_function
    return log_this