import os
import torch
import re
import pandas
torch.__version__
import torchvision.datasets
torchvision.__version__
from torchvision.transforms import ToTensor
inside="""(?P<split_val>[^(),`'" ]*?)"""
split_val_patterns=[q+inside+q for q in ("'","``",'"')]
for p in split_val_patterns:
    print(p)
not_again='(?! +[^ ]+ [(])'
arg_pattern = ''.join([
    ' +',
    '(?P<arg_name>[^ ]+)',
    ' [(]',
    '(?P<paren>.*?)',
    '[)]:',
    '(?P<doc>.*\n(?:%s.*\n)*)'%not_again
    ])
def get_arg_dict(doc_string):
    doc_dict = {}
    if doc_string is None:
        return doc_dict
    for m in re.finditer(arg_pattern, doc_string):
        doc_dict[ m.groupdict()["arg_name"] ] = m.groupdict()["doc"]
    return doc_dict
[get_arg_dict(s)["split"] for s in ['`The Rendered SST2 Dataset <https://github.com/openai/CLIP/blob/main/data/rendered-sst2.md>`_.\n\n    Rendered SST2 is an image classification dataset used to evaluate the models capability on optical\n    character recognition. This dataset was generated by rendering sentences in the Standford Sentiment\n    Treebank v2 dataset.\n\n    This dataset contains two classes (positive and negative) and is divided in three splits: a  train\n    split containing 6920 images (3610 positive and 3310 negative), a validation split containing 872 images\n    (444 positive and 428 negative), and a test split containing 1821 images (909 positive and 912 negative).\n\n    Args:\n        root (string): Root directory of the dataset.\n        split (string, optional): The dataset split, supports ``"train"`` (default), `"val"` and ``"test"``.\n        transform (callable, optional): A function/transform that  takes in an PIL image and returns a transformed\n            version. E.g, ``transforms.RandomCrop``.\n        target_transform (callable, optional): A function/transform that takes in the target and transforms it.\n        download (bool, optional): If True, downloads the dataset from the internet and\n            puts it in root directory. If dataset is already downloaded, it is not\n            downloaded again. Default is False.\n    ', "`WIDERFace <http://shuoyang1213.me/WIDERFACE/>`_ Dataset.\n\n    Args:\n        root (string): Root directory where images and annotations are downloaded to.\n            Expects the following folder structure if download=False:\n\n            .. code::\n\n                <root>\n                    \u2514\u2500\u2500 widerface\n                        \u251c\u2500\u2500 wider_face_split ('wider_face_split.zip' if compressed)\n                        \u251c\u2500\u2500 WIDER_train ('WIDER_train.zip' if compressed)\n                        \u251c\u2500\u2500 WIDER_val ('WIDER_val.zip' if compressed)\n                        \u2514\u2500\u2500 WIDER_test ('WIDER_test.zip' if compressed)\n        split (string): The dataset split to use. One of {``train``, ``val``, ``test``}.\n            Defaults to ``train``.\n        transform (callable, optional): A function/transform that  takes in a PIL image\n            and returns a transformed version. E.g, ``transforms.RandomCrop``\n        target_transform (callable, optional): A function/transform that takes in the\n            target and transforms it.\n        download (bool, optional): If true, downloads the dataset from the internet and\n            puts it in root directory. If dataset is already downloaded, it is not\n            downloaded again.\n\n    ", '`EMNIST <https://www.westernsydney.edu.au/bens/home/reproducible_research/emnist>`_ Dataset.\n\n    Args:\n        root (string): Root directory of dataset where ``EMNIST/raw/train-images-idx3-ubyte``\n            and  ``EMNIST/raw/t10k-images-idx3-ubyte`` exist.\n        split (string): The dataset has 6 different splits: ``byclass``, ``bymerge``,\n            ``balanced``, ``letters``, ``digits`` and ``mnist``. This argument specifies\n            which one to use.\n        train (bool, optional): If True, creates dataset from ``training.pt``,\n            otherwise from ``test.pt``.\n        download (bool, optional): If True, downloads the dataset from the internet and\n            puts it in root directory. If dataset is already downloaded, it is not\n            downloaded again.\n        transform (callable, optional): A function/transform that  takes in an PIL image\n            and returns a transformed version. E.g, ``transforms.RandomCrop``\n        target_transform (callable, optional): A function/transform that takes in the\n            target and transforms it.\n    ']]
train_arg_dict = {
    "EMNIST":{"split":"mnist"}
}
split_arg_dict = {
    'STL10': ('test', 'train'),
    'Cityscapes':{
        "mode":"fine",
        "split":("train","test","val")
    },
    'LFWPairs': ('train', 'test'),
    'MovingMNIST': ('train', 'test')
}
class_data_names = [
    "Caltech101",
    "Caltech256",
    "CelebA",
    "CIFAR10",
    "CIFAR100",
    "Country211",
    "DTD",
    "EMNIST",
    "EuroSAT",
    "FashionMNIST",
    "FER2013",
    "FGVCAircraft",
    "Flickr8k",
    "Flickr30k",
    "Flowers102",
    "Food101",
    "GTSRB",
    "INaturalist",
    "ImageNet",
    "Imagenette",
    "KMNIST",
    "LFWPeople",
    "LSUN",
    "MNIST",
    "Omniglot",
    "OxfordIIITPet",
    "Places365",
    "PCAM",
    "QMNIST",
    "RenderedSST2",
    "SEMEION",
    "SBU",
    "StanfordCars",
    "STL10",
    "SUN397",
    "SVHN",
    "USPS",
]
too_big = [
    "USPS",#redundant with zip data set
    "SVHN",
    'FGVCAircraft',
    'Flowers102',
    "Food101",
    'CLEVRClassification',
    "CelebA",#RuntimeError: The MD5 checksum of the download file data/CelebA/celeba/img_align_celeba.zip does not match the one on record.Please delete the file and try again. If the issue persists, please report this to torchvision at https://github.com/pytorch/vision/issues.
    "Country211",
    "DTD",#RuntimeError: stack expects each tensor to be equal size, but got [3, 600, 600] at entry 0 and [3, 500, 500] at entry 1
    "GTSRB",
    'RenderedSST2',
    "PCAM",
    "Kinetics",
    "Kitti",
    "LFWPairs",
    "StanfordCars",#urllib.error.HTTPError: HTTP Error 404: Not Found
    "LFWPeople",
    'Places365',
    "OxfordIIITPet",#RuntimeError: stack expects each tensor to be equal size, but got [3, 500, 394] at entry 0 and [3, 313, 450] at entry 1
    "Imagenette",#RuntimeError: stack expects each tensor to be equal size, but got [3, 375, 500] at entry 0 and [3, 500, 375] at entry 6
]
for data_name in class_data_names:
    data_class = getattr(torchvision.datasets, data_name)
    ann_dict = getattr(data_class.__init__, "__annotations__", {})
    doc_string = data_class.__doc__
    doc_dict = get_arg_dict(doc_string)
    cache_dir = os.path.join("data", data_name)
    out_csv = cache_dir+".csv"
    if not os.path.exists(out_csv) and "download" in doc_dict and not data_name in too_big and ("train" in doc_dict or "split" in doc_dict):
        data_df_list = []
        class_kwargs = {
            "root":cache_dir,
            "download":True,
            "transform":ToTensor()
        }
        if "train" in doc_dict:
            more_kwargs = train_arg_dict.get(data_name, {})
            class_kwargs.update(more_kwargs)
            for train in True, False:
                class_kwargs["train"]=train
                data_inst = data_class(**class_kwargs)
                print(data_inst)
                dl = torch.utils.data.DataLoader(data_inst, batch_size=len(data_inst), shuffle=False)
                for b in dl:
                    input_tensor, output_tensor = b
                    data_df_list.append(pandas.concat([
                        pandas.DataFrame({
                            "predefined.set":"train" if train else "test",
                            "y":output_tensor,
                        }),
                        pandas.DataFrame(input_tensor.flatten(start_dim=1))
                    ], axis=1))
        elif "split" in doc_dict:
            split_doc = doc_dict["split"]
            split_info = split_arg_dict.get(data_name)
            suffix = None
            if split_info is None:
                split_val_list = []
                for pattern in split_val_patterns:
                    for m in re.finditer(pattern, split_doc):
                        split_val_list.append(m.groupdict()["split_val"])
            else:
                try:
                    split_val_list = split_info.pop("split")
                    class_kwargs.update(split_info)
                except AttributeError:
                    split_val_list = split_info
            split_val_set = set(split_val_list)
            print({data_name:(split_val_set, split_doc)})
            for split in split_val_set:
                class_kwargs["split"]=split
                data_inst = data_class(**class_kwargs)
                print(data_inst)
                dl = torch.utils.data.DataLoader(data_inst, batch_size=len(data_inst), shuffle=False)
                for b in dl:
                    input_tensor, output_tensor = b
                    data_df_list.append(pandas.concat([
                        pandas.DataFrame({
                            "predefined.set":split,
                            "y":output_tensor,
                        }),
                        pandas.DataFrame(input_tensor.flatten(start_dim=1))
                    ], axis=1))
        data_df = pandas.concat(data_df_list)
        data_df.to_csv(out_csv,index=False)