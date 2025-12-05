"""Configuration and paths module translated from paths.ml."""

import os
from pathlib import Path


class Config:
    """Global configuration settings."""

    # Platform info
    PLATFORM = "Station"
    DEFAULT_TRANSLITERATION = "VH"
    DEFAULT_LEXICON = "MW"
    DEFAULT_DISPLAY_FONT = "roma"

    # Installation directories
    ZEN_INSTALL_DIR = "./Zen/ML"
    SKT_INSTALL_DIR = os.path.dirname(os.path.abspath(__file__)) + "/../.."
    SKT_RESOURCES_DIR = "./Heritage_Resources/"
    PUBLIC_SKT_DIR = "./sanskrit/"
    SKT_DIR_URL = "./html/"

    # Server configuration
    SERVER_HOST = "127.0.0.1"
    REMOTE_SERVER_HOST = "https://sanskrit.inria.fr/"

    # CGI URLs and entry points
    CGI_DIR_URL = "./SKT/"
    CGI_INDEX = "sktindex"
    CGI_INDEXD = "sktsearch"
    CGI_LEMMATIZER = "sktlemmatizer"
    CGI_READER = "sktreader"
    CGI_PARSER = "sktparser"
    CGI_TAGGER = "skttagger"
    CGI_DECL = "sktdeclin"
    CGI_CONJ = "sktconjug"
    CGI_SANDHIER = "sktsandhier"
    CGI_GRAPH = "sktgraph"
    CGI_USER_AID = "sktuser"
    CGI_CORPUS_MANAGER = "sktcorpus"
    CGI_SAVE_CORPUS = "savecorpus"
    CGI_MKDIR_CORPUS = "mkdircorpus"

    # UI settings
    MOUSE_ACTION = "CLICK"


class Params:
    """Parameter names for CGI."""

    CORPUS_DIR = "corpdir"
    SENTENCE_NO = "sentno"
    CORPUS_PERMISSION = "corpmode"


def get_install_dir(subdir: str = "") -> Path:
    """Get installation directory."""
    base = Path(Config.SKT_INSTALL_DIR)
    if subdir:
        return base / subdir
    return base


def get_resource_dir(subdir: str = "") -> Path:
    """Get resource directory."""
    base = Path(Config.SKT_RESOURCES_DIR)
    if subdir:
        return base / subdir
    return base


def get_data_dir(subdir: str = "") -> Path:
    """Get data directory."""
    base = get_install_dir("DATA")
    if subdir:
        return base / subdir
    return base
