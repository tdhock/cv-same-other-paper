import os
import torch
import re
import pandas
torch.__version__
import torchvision.datasets
torchvision.__version__
from torchvision.transforms import ToTensor
class_data_names = [
    "CIFAR10",
    "EMNIST",
    "FashionMNIST",
    "KMNIST",
    "MNIST",
    "QMNIST",
]
for data_name in class_data_names:
    data_class = getattr(torchvision.datasets, data_name)
    ann_dict = getattr(data_class.__init__, "__annotations__", {})
    doc_string = data_class.__doc__
    doc_dict = get_arg_dict(doc_string)
    cache_dir = os.path.join("data_Classif", data_name)
    out_csv = cache_dir+".csv"
    if not os.path.exists(out_csv):
        data_df_list = []
        class_kwargs = {
            "root":cache_dir,
            "download":True,
            "transform":ToTensor()
        }
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
        data_df = pandas.concat(data_df_list)
        data_df.to_csv(out_csv,index=False)
