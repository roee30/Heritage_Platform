"""Interface module - Sanskrit Reader summarizing interface (CGI handler).

Translated from interface.ml. This is the main entry point that processes CGI requests
for Sanskrit text segmentation and morphological analysis.
"""
from __future__ import annotations

import os
from typing import Dict, List, Optional, Any
from io import StringIO
import urllib.parse

from .word import Word
from .config import Config, Params
from .canon import uniromcode, unidevcode
from .phases import Phase
from .stubs import (
    Morphology, Dispatcher, Segmenter, Graph, Html, Web, Cgi, Encode, Sanskrit, Checkpoints, MaybeEnv, _DEFAULT_LEXICON
)



class Interface:
    """Main CGI interface for Sanskrit morphological analysis."""
    
    def __init__(self):
        """Initialize the interface."""
        self.output = StringIO()
        Web.set_output(self.output)
        
        # Morphology and dispatcher
        self.morphology = Morphology()
        self.dispatcher = Dispatcher()
        self.segmenter = Segmenter()
        self.graph = Graph()
        
        # Visual display state
        self.visual = {}
        self.visual_width = {}
        self.visual_conf = {}
        self.max_col = 0
        
        # Segmentation state
        self.cur_chunk = {"offset": 0}
        self.current_language = "en"
        self.remote = False
        # inferred text length (in internal code units)
        self.text_length = 0
        # inferred text length (in internal code units)
        self.text_length = 0
    
    def ps(self, text: str) -> None:
        """Print string to output."""
        self.output.write(text)
    
    def pl(self, text: str) -> None:
        """Print string with newline to output."""
        self.output.write(text + "\n")
    
    def get_output(self) -> str:
        """Get accumulated output."""
        return self.output.getvalue()
    
    def http_header(self) -> str:
        """Generate HTTP header."""
        return "Content-Type: text/html; charset=UTF-8\n\n"
    
    def page_begin(self, title: str = "Sanskrit Interface") -> str:
        """Generate HTML page beginning."""
        return f"""<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>{title}</title>
    <style>
        body {{ font-family: Arial, sans-serif; }}
        table {{ border-collapse: collapse; }}
        td {{ border: 1px solid #ccc; padding: 5px; }}
        .tooltip {{ position: relative; display: inline-block; }}
        .tooltiptext {{ visibility: hidden; background-color: #555; color: #fff; 
                       text-align: center; padding: 5px; border-radius: 6px; 
                       position: absolute; z-index: 1; }}
        .tooltip:hover .tooltiptext {{ visibility: visible; }}
    </style>
</head>
<body>
"""
    
    def page_end(self) -> str:
        """Generate HTML page ending."""
        return """</body>
</html>
"""
    
    def sort_check(self, checkpoints: List) -> List:
        """Sort checkpoints by index."""
        if not checkpoints:
            return []
        return sorted(checkpoints, key=lambda x: x[0])
    
    def seg_length(self, word: Word) -> int:
        """Get segment length."""
        if not word or len(word) == 0:
            return 0
        return len(word)
    
    def build_visual(self, k: int, segments: List) -> None:
        """Build visual representation at offset k."""
        if not segments:
            return
        
        # Merge segments for display
        self.visual[k] = segments
        total_length = sum(self.seg_length(w[0]) for w in segments)
        self.visual_width[k] = k + total_length
    
    def print_morpho(self, phase: Phase, word: Word) -> str:
        """Print morphological analysis for a word."""
        output = []
        tags = self.morphology.tags_of(phase, word)
        if tags:
            for tag in tags:
                output.append(str(tag))
        return "\n".join(output) if output else ""
    
    def print_word(self, last_ind: int, text: str, checkpoints: List,
                  word: Word, phase: Phase, k: int, conflict: bool) -> str:
        """Print a word with its morphological analysis."""
        output = []
        
        # Calculate extra spacing
        extra_space = k - last_ind
        if extra_space > 0:
            output.append(Html.td_wrap("&nbsp;" * extra_space))
        
        # Print word in table cell
        word_text = uniromcode(word.elements if isinstance(word, Word) else word)
        morph = self.print_morpho(phase, word)
        
        output.append(Html.table_begin())
        output.append(Html.tr_begin())
        output.append(f"<td>{word_text}</td>")
        if morph:
            output.append(f"<td class='tooltip'>{word_text}<span class='tooltiptext'>{morph}</span></td>")
        output.append(Html.tr_end())
        output.append(Html.table_end())
        
        return "\n".join(output)
    
    def print_row(self, text: str, checkpoints: List) -> str:
        """Print a row of segments."""
        output = []
        output.append(Html.tr_begin())
        
        # Placeholder for visual row printing
        output.append(Html.td_wrap("Row"))
        
        output.append(Html.tr_end())
        return "\n".join(output)
    
    def print_interf(self, text: str, checkpoints: List) -> str:
        """Print the main interface table showing canonical segmentations.

        This enumerates complete segmentation paths through the visual graph,
        filters paths to keep only those composed of lexicon words, and
        renders each canonical segmentation as a table row.
        """
        output = []
        output.append(Html.table_begin("results"))

        # Reconstruct graph from visual (offset -> list of (Word, meta))
        graph = {}
        for offset, segs in self.visual.items():
            graph[offset] = []
            for s in segs:
                w = s[0] if isinstance(s, tuple) else s
                meta = s[1] if isinstance(s, tuple) and len(s) > 1 else []
                graph[offset].append((w, meta))

        if not graph or self.text_length <= 0:
            output.append(Html.tr_begin())
            output.append(Html.td_wrap("No segmentation paths found."))
            output.append(Html.tr_end())
            output.append(Html.table_end())
            return "\n".join(output)

        # DFS to enumerate full paths from offset 0 to text_length
        paths = []
        max_paths = 200

        def dfs(offset: int, cur: List):
            if len(paths) >= max_paths:
                return
            if offset == self.text_length:
                paths.append(list(cur))
                return
            if offset not in graph:
                return
            for word, meta in graph[offset]:
                codes = word.elements if isinstance(word, Word) else word
                l = len(codes)
                next_off = offset + l
                if next_off <= self.text_length:
                    cur.append((word, offset))
                    dfs(next_off, cur)
                    cur.pop()

        dfs(0, [])

        # Filter to canonical lexicon-based paths
        lex = set(_DEFAULT_LEXICON) if _DEFAULT_LEXICON else set()
        canonical = []
        for p in paths:
            ok = True
            for word, off in p:
                codes = word.elements if isinstance(word, Word) else word
                latin = uniromcode(codes)
                if latin not in lex:
                    ok = False
                    break
            if ok:
                canonical.append(p)

        # If none canonical, fall back to showing a small sample of all paths
        to_show = canonical if canonical else paths[:20]

        for idx, p in enumerate(to_show):
            parts = []
            for w, off in p:
                codes = w.elements if isinstance(w, Word) else w
                parts.append(Html.escape(uniromcode(codes)))
            output.append(Html.tr_begin())
            output.append(Html.td_wrap(f"Segmentation {idx+1}"))
            output.append(Html.td_wrap(" + ".join(parts)))
            output.append(Html.tr_end())

        output.append(Html.table_end())
        return "\n".join(output)
    
    def check_sentence(self, translit: str, uns: bool, text: str,
                      checkpoints: List, input_text: str,
                      undo_enabled: bool = False) -> None:
        """Main segmentation procedure.
        
        Args:
            translit: Transliteration scheme (VH, WX, SLP1)
            uns: Whether to process without sandhi splitting
            text: Text parameters for CGI
            checkpoints: Current checkpoint selections
            input_text: The Sanskrit text to process
            undo_enabled: Whether undo button is enabled
        """
        try:
            encode = Encode.switch_code(translit)
            
            # Read and chunk Sanskrit text
            if uns:
                chunks = Sanskrit.read_raw_sanskrit(encode, input_text)
            else:
                chunks = Sanskrit.read_sanskrit(encode, input_text)
            
            raw_chunks = Sanskrit.read_raw_sanskrit(encode, input_text)
            # Render Devanagari from the full raw chunk list so that leading
            # initial 'a' vowels in non-first words can be suppressed when
            # joining (prevents an extra 'अ' between consecutive words).
            deva_input = unidevcode(raw_chunks) if raw_chunks else ""
            
            # Sort checkpoints
            cpts = self.sort_check(checkpoints)
            
            # Compute total text length (sum of chunk lengths)
            self.text_length = sum(len(c.elements) if isinstance(c, Word) else len(c) for c in chunks)

            # Perform segmentation
            full, count = self.segmenter.segment_iter(chunks)

            # If the segmenter produced a graph, use it to build visuals
            seg_graph = getattr(self.segmenter, 'graph', None)
            if seg_graph:
                # seg_graph: offset -> list of (Word, ...)
                for off in sorted(seg_graph.keys()):
                    segs = seg_graph[off]
                    # Ensure segments are tuples where first element is Word
                    formatted = [s if isinstance(s, tuple) else (s, []) for s in segs]
                    self.build_visual(off, formatted)
            
            # Build visual representation
            self.pl(Html.h3_section(f"Sanskrit Text Analysis ({count} solutions)"))
            self.pl(f"<p>Input: {Html.escape(input_text)}</p>")
            self.pl(f"<p>Devanagari: {deva_input}</p>")
            
            # Display segmentation table (use stored text_length)
            self.pl(self.print_interf(text, checkpoints))
            
            # Display control buttons
            self.pl(f"<p>Full segmentation: {full}</p>")
            
        except Exception as e:
            raise
            self.pl(f"<p><b>Error:</b> {Html.escape(str(e))}</p>")
    
    def parse_cgi_params(self, env: MaybeEnv) -> Dict[str, Any]:
        """Parse CGI parameters from environment."""
        return {
            'text': Cgi.get('text', env, ''),
            'translit': Cgi.get('t', env, Config.DEFAULT_TRANSLITERATION),
            'lex': Cgi.get('lex', env, Config.DEFAULT_LEXICON),
            'font': Cgi.get('font', env, Config.DEFAULT_DISPLAY_FONT),
            'cache': Cgi.get('cache', env, '1'),
            'st': Cgi.get('st', env, ''),
            'us': Cgi.get('us', env, '0'),
            'topic': Cgi.get('topic', env, ''),
            'abs': Cgi.get('abs', env, '0'),
            'cpts': Cgi.get('cpts', env, ''),
            'revised': Cgi.get('revised', env, ''),
            'rev_off': Cgi.get('rev_off', env, '-1'),
            'rev_ind': Cgi.get('rev_ind', env, '-1'),
            'corpus_permission': Cgi.get(Params.CORPUS_PERMISSION, env, ''),
            'corpus_dir': Cgi.get(Params.CORPUS_DIR, env, ''),
            'sentence_no': Cgi.get(Params.SENTENCE_NO, env, ''),
        }
    
    def graph_engine(self, env: MaybeEnv = None) -> str:
        """Main graph engine - processes CGI request.
        
        Args:
            env: CGI environment dict (or read from sys.environ if None)
        
        Returns:
            HTML output string
        """
        if env is None:
            env = dict(os.environ)
        
        # Parse parameters
        params = self.parse_cgi_params(env)
        
        # Extract parameters
        text = params['text']
        translit = params['translit']
        uns = params['us'] == '1'
        input_text = urllib.parse.unquote(text) if text else ""
        checkpoints = Checkpoints.parse_points(params['cpts'])
        
        # Generate page
        self.pl(self.http_header())
        self.pl(self.page_begin("Sanskrit Heritage Platform - Interface"))
        
        self.pl(Html.h3_section("Sanskrit Reader Interface"))
        self.pl(f"<p>Processing: {Html.escape(input_text)}</p>")
        
        # Main processing
        self.check_sentence(translit, uns, text, checkpoints, input_text, False)
        
        self.pl(self.page_end())
        
        return self.get_output()
    
    def safe_engine(self, env: MaybeEnv = None) -> str:
        """Safe wrapper for graph engine with error handling.
        
        Args:
            env: CGI environment dict
        
        Returns:
            HTML output string
        """
        try:
            return self.graph_engine(env)
        except Exception as e:
            raise
            # Generate error page
            output = StringIO()
            output.write(self.http_header())
            output.write(self.page_begin("Error"))
            output.write(f"<h2>Error</h2>")
            output.write(f"<p><b>{type(e).__name__}:</b> {Html.escape(str(e))}</p>")
            output.write(self.page_end())
            return output.getvalue()


# Singleton instance
interface_instance = Interface()


def process_request(env: Optional[dict[str, str]] = None) -> str:
    """Process a CGI request and return HTML.
    
    Args:
        env: CGI environment dict (or read from sys.environ if None)
    
    Returns:
        HTML output string
    """
    return interface_instance.safe_engine(env)
