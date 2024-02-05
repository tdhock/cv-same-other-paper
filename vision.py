import torch
import re
torch.__version__
import torchvision.datasets
torchvision.__version__
split_val_patterns=[
    '"(?P<split_val>.*?)"',
    "'(?P<split_val>.*?)'",
    """``(?P<split_val>[^'"]*?)``""",
]
import os
for data_name in dir(torchvision.datasets):
    data_class = getattr(torchvision.datasets, data_name)
    ann_dict = getattr(data_class.__init__, "__annotations__", {})
    doc_string = data_class.__doc__
    if type(doc_string) is type(""):
        out_txt=os.path.expanduser("~/teaching/regex-tutorial/torchvision-docs/"+data_name+".txt")
        f=open(out_txt, "w", encoding="utf-8")
        f.write(doc_string)
        f.close()
        doc_dict = {}
        for m in re.finditer('\n +(?P<arg_name>[^ ]+) [(](?P<paren>.*?)[)]:(?P<doc>.*?)[.]', doc_string, re.DOTALL):
            doc_dict[ m.groupdict()["arg_name"] ] = m.groupdict()["doc"]
        print({data_name:doc_dict.keys()})
        if "split" in doc_dict and "train" in doc_dict:
            print(data_name)
    if "split" in ann_dict and False:
        m = re.findall('(?<arg_name>[^ ]+) [(](?P<paren>.*?)[)]:(?P<doc>.*?)[.]', data_class.__doc__, re.DOTALL)
        split_doc = m.groupdict()["after"]
        split_val_list = []
        for pattern in split_val_patterns:
            for m in re.finditer(pattern, split_doc):
                split_val_list.append(m.groupdict()["split_val"])
        print({data_name:split_val_list})
